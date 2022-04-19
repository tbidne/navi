{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides a mock Navi implementation.
module Integration.MockApp
  ( MockEnv (..),
    runMockApp,
  )
where

import Integration.Prelude
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects.MonadLogger (MonadLogger (..))
import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Effects.MonadNotify (MonadNotify (..))
import Navi.Effects.MonadShell (MonadShell (..))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (..))
import Navi.Env.Core (HasEvents (..), HasPollInterval (..))
import Navi.Event.Types (AnyEvent)
import Navi.NaviT (NaviT (..), runNaviT)
import Navi.Services.Types (ServiceType (..))
import Numeric.Data.Interval qualified as Interval
import Pythia.Control.Exception (CommandException (..))
import Pythia.Services.Battery (Battery (..), BatteryPercentage (..), BatteryStatus (..))

-- | Mock configuration.
data MockEnv = MkMockEnv
  { pollInterval :: Word16,
    events :: NonEmpty (AnyEvent IORef),
    -- | "Sent" notifications are captured in this ref rather than
    -- actually sent. This way we can later test what was sent.
    sentNotes :: IORef [NaviNote],
    -- | caches the last battery percentage "reading". This way we can
    -- ensure we have a new percentage every time.
    lastPercentage :: IORef BatteryPercentage
  }

makeFieldLabelsNoPrefix ''MockEnv

instance HasEvents IORef MockEnv where
  getEvents = events

instance HasPollInterval MockEnv where
  getPollInterval = pollInterval

newtype IntTestIO a = MkIntTestIO {runIntTestIO :: IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadMutRef IORef,
      MonadShell,
      MonadThrow
    )
    via IO

instance MonadLogger (NaviT MockEnv IntTestIO) where
  logFm _ _ = pure ()

  -- if we ever decide to test logs, we can capture them similar to the
  -- MonadNotify instance.
  logText _ _ = pure ()
  addNamespace _ mx = mx

instance MonadNotify (NaviT MockEnv IntTestIO) where
  sendNote note = do
    notes <- asks (view #sentNotes)
    liftIO $ modifyIORef' notes (note :)

instance MonadSystemInfo (NaviT MockEnv IntTestIO) where
  -- Service that changes every time: can be used to test multiple
  -- notifications are sent.
  query (BatteryPercentage _) = do
    bpRef <- asks (view #lastPercentage)
    oldVal <- liftIO $ Interval.unLRInterval . unBatteryPercentage <$> readIORef bpRef
    let !newVal =
          if oldVal == 0
            then 100
            else oldVal - 1
        newBp = MkBatteryPercentage $ Interval.unsafeLRInterval newVal
    liftIO $ writeIORef bpRef newBp
    pure $ MkBattery newBp Discharging
  -- Constant service. Can test duplicate behavior.
  query (BatteryStatus _) = pure Charging
  -- Service error. Can test error behavior.
  query (NetworkInterface _ _) =
    throw $ MkCommandException "nmcli" "Nmcli error"
  -- Constant service. Can test duplicate behavior.
  query (Single _) = pure "single trigger"
  -- Constant service. Can test duplicate behavior.
  query (Multiple _) = pure "multiple result"

runMockApp :: (NaviT MockEnv IntTestIO) a -> MockEnv -> IO a
runMockApp nt = runIntTestIO . runNaviT nt
