{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides a mock Navi implementation.
module Integration.MockApp
  ( MockEnv (..),
    runMockApp,
    configToMockEnv,
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Integration.Prelude
import Katip.Core (Namespace)
import Navi.Config (Config)
import Navi.Data.NaviLog (NaviLog)
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Data.NaviQueue (NaviQueue (MkNaviQueue))
import Navi.Effects.MonadLogger (MonadLogger (..))
import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Effects.MonadNotify (MonadNotify (..))
import Navi.Effects.MonadQueue (MonadQueue (..))
import Navi.Effects.MonadShell (MonadShell (..))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (..))
import Navi.Env.Core
  ( HasEvents (..),
    HasLogNamespace (..),
    HasLogQueue (..),
    HasNoteQueue (..),
  )
import Navi.Event.Types (AnyEvent, EventError (MkEventError))
import Navi.NaviT (NaviT (..), runNaviT)
import Navi.Services.Types (ServiceType (..))
import Numeric.Data.Interval qualified as Interval
import Pythia.Control.Exception (CommandException (..))
import Pythia.Services.Battery (Battery (..), BatteryStatus (..), Percentage (..))

-- | Mock configuration.
data MockEnv = MkMockEnv
  { events :: !(NonEmpty (AnyEvent IORef)),
    logQueue :: !(NaviQueue (NaviLog, Namespace)),
    noteQueue :: !(NaviQueue NaviNote),
    -- | "Sent" notifications are captured in this ref rather than
    -- actually sent. This way we can later test what was sent.
    sentNotes :: !(IORef [NaviNote]),
    -- | caches the last battery percentage "reading". This way we can
    -- ensure we have a new percentage every time.
    lastPercentage :: !(IORef Percentage)
  }

makeFieldLabelsNoPrefix ''MockEnv

instance HasEvents IORef MockEnv where
  getEvents = view #events

instance HasLogNamespace MockEnv where
  getLogNamespace _ = ""
  setLogNamespace _ = id
  overLogNamespace _ = id

instance HasLogQueue MockEnv where
  getLogQueue = view #logQueue

instance HasNoteQueue MockEnv where
  getNoteQueue = view #noteQueue

newtype IntTestIO a = MkIntTestIO (IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadQueue,
      MonadMutRef IORef,
      MonadShell,
      MonadUnliftIO
    )
    via IO

makePrisms ''IntTestIO

instance MonadLogger (NaviT MockEnv IntTestIO) where
  -- if we ever decide to test logs, we can capture them similar to the
  -- MonadNotify instance.
  logText _ _ = pure ()
  addNamespace _ mx = mx

instance MonadNotify (NaviT MockEnv IntTestIO) where
  sendNote note =
    if note ^. #summary == "SentException"
      then throwIO $ MkEventError "SentException" "sending mock exception" ""
      else do
        notes <- asks (view #sentNotes)
        liftIO $ modifyIORef' notes (note :)

instance MonadSystemInfo (NaviT MockEnv IntTestIO) where
  -- Service that changes every time: can be used to test multiple
  -- notifications are sent.
  query (BatteryPercentage _) = do
    bpRef <- asks (view #lastPercentage)
    oldVal <- liftIO $ Interval.unLRInterval . unPercentage <$> readIORef bpRef
    let !newVal =
          if oldVal == 0
            then 100
            else oldVal - 1
        newBp = MkPercentage $ Interval.unsafeLRInterval newVal
    liftIO $ writeIORef bpRef newBp
    pure $ MkBattery newBp Discharging
  -- Constant service. Can test duplicate behavior.
  query (BatteryStatus _) = pure Charging
  -- Service error. Can test error behavior.
  query (NetworkInterface _ _) =
    throwIO $ MkCommandException "nmcli" "Nmcli error"
  -- Constant service. Can test duplicate behavior.
  query (Single _) = pure "single trigger"
  -- Constant service. Can test duplicate behavior.
  query (Multiple _) = pure "multiple result"

runMockApp :: (NaviT MockEnv IntTestIO) a -> MockEnv -> IO a
runMockApp nt = view _MkIntTestIO . runNaviT nt

configToMockEnv :: Config IORef -> IO MockEnv
configToMockEnv config = do
  sentNotesRef <- newIORef []

  lastPercentageRef <- newIORef $ MkPercentage $ Interval.unsafeLRInterval 6
  logQueue <- liftIO $ STM.atomically $ TBQueue.newTBQueue 1000
  noteQueue <- liftIO $ STM.atomically $ TBQueue.newTBQueue 1000

  pure $
    MkMockEnv
      { events = config ^. #events,
        sentNotes = sentNotesRef,
        logQueue = MkNaviQueue logQueue,
        noteQueue = MkNaviQueue noteQueue,
        lastPercentage = lastPercentageRef
      }
