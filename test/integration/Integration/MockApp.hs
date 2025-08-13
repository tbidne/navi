{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides a mock Navi implementation.
module Integration.MockApp
  ( MockEnv (..),
    runMockApp,
    runMockAppEnv,
  )
where

import Control.Concurrent qualified as CC
import Effects.Concurrent.Async qualified as Async
import FileSystem.OsPath (decodeLenient)
import Integration.Prelude
import Navi (runNavi)
import Navi.Data.NaviNote (CustomResult, NaviNote, parseCustomResult)
import Navi.Effects.MonadNotify (MonadNotify (sendNote))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (query))
import Navi.Env.Core
  ( CoreEnvField (MkCoreEnvField),
    Env,
    HasEvents,
    HasLogEnv,
    HasNoteQueue,
  )
import Navi.Event.Types (EventError (MkEventError))
import Navi.Runner qualified as Runner
import Navi.Services.Types
  ( ServiceType
      ( BatteryPercentage,
        BatteryStatus,
        Multiple,
        NetworkInterface,
        Single
      ),
  )
import Pythia.Control.Exception (CommandException (MkCommandException))
import Pythia.Data.Percentage qualified as Percentage
import Pythia.Services.Battery
  ( Battery (MkBattery),
    BatteryStatus (Charging, Discharging),
    Percentage,
  )
import System.Environment qualified as SysEnv

-- | Mock configuration.
data MockEnv = MkMockEnv
  { coreEnv :: Env,
    -- | "Sent" notifications are captured in this ref rather than
    -- actually sent. This way we can later test what was sent.
    sentNotes :: IORef [NaviNote],
    -- | caches the last battery percentage "reading". This way we can
    -- ensure we have a new percentage every time.
    lastPercentage :: IORef Percentage,
    multipleResponses :: IORef [Text],
    singleResponses :: IORef [Text]
  }

makeFieldLabelsNoPrefix ''MockEnv

instance
  ( k ~ A_Lens,
    x ~ Namespace,
    y ~ Namespace
  ) =>
  LabelOptic "namespace" k MockEnv MockEnv x y
  where
  labelOptic =
    lensVL $ \f env ->
      fmap
        (const env)
        (f "")
  {-# INLINE labelOptic #-}

deriving via (CoreEnvField MockEnv) instance HasEvents MockEnv

deriving via (CoreEnvField MockEnv) instance HasLogEnv MockEnv

deriving via (CoreEnvField MockEnv) instance HasNoteQueue MockEnv

newtype MockAppT a = MkMockAppT (ReaderT MockEnv IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadAsync,
      MonadCatch,
      MonadFileReader,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadReader MockEnv,
      MonadSTM,
      MonadTerminal,
      MonadThread,
      MonadThrow,
      MonadTypedProcess
    )
    via (ReaderT MockEnv IO)

runMockAppT :: MockAppT a -> MockEnv -> IO a
runMockAppT (MkMockAppT rdr) = runReaderT rdr

instance MonadLogger MockAppT where
  -- if we ever decide to test logs, we can capture them similar to the
  -- MonadNotify instance.
  monadLoggerLog _loc _src _lvl _msg = pure ()

instance MonadNotify MockAppT where
  sendNote note =
    if note ^. #summary == "SentException"
      then throwM $ MkEventError "SentException" "sending mock exception" ""
      else do
        notes <- asks (view #sentNotes)
        liftIO $ modifyIORef' notes (note :)

instance MonadSystemInfo MockAppT where
  -- Service that changes every time: can be used to test multiple
  -- notifications are sent.
  query (BatteryPercentage _) = do
    bpRef <- asks (view #lastPercentage)
    oldVal <- Percentage.unPercentage <$> readIORef bpRef
    let !newVal =
          if oldVal == 0
            then 100
            else oldVal - 1
        newBp = Percentage.unsafePercentage newVal
    liftIO $ writeIORef bpRef newBp
    pure $ MkBattery newBp Discharging
  -- Constant service. Can test duplicate behavior.
  query (BatteryStatus _) = pure Charging
  -- Service error. Can test error behavior.
  query (NetworkInterface _ _) =
    throwM $ MkCommandException "nmcli" "Nmcli error"
  query (Single _) = getResponseOrDefault #singleResponses "single trigger"
  query (Multiple _) = getResponseOrDefault #multipleResponses "multiple result"

getResponseOrDefault ::
  Lens' MockEnv (IORef [Text]) ->
  Text ->
  MockAppT CustomResult
getResponseOrDefault l def =
  parseCustomResult <$> do
    ref <- asks (view l)
    singleResponses <- readIORef ref
    case singleResponses of
      [] -> pure def
      r : rs -> do
        writeIORef ref rs
        pure r

runMockApp :: Word8 -> OsPath -> IO MockEnv
runMockApp = runMockAppEnv pure

runMockAppEnv :: (MockEnv -> IO MockEnv) -> Word8 -> OsPath -> IO MockEnv
runMockAppEnv modEnv maxSeconds configPath = do
  lastPercentage <- newIORef $ Percentage.unsafePercentage 6
  multipleResponses <- newIORef []
  singleResponses <- newIORef []
  sentNotes <- newIORef []

  let action = SysEnv.withArgs args $ Runner.withEnv $ \coreEnv -> do
        let env =
              MkMockEnv
                { coreEnv,
                  lastPercentage,
                  sentNotes,
                  multipleResponses,
                  singleResponses
                }
        env' <- modEnv env
        Async.race
          (countdown maxSeconds $> env')
          (runMockAppT (absurd <$> runNavi) env')

  result <- action
  case result of
    Left env -> pure env
    Right env -> pure env
  where
    args = ["-c", path]

    path = decodeLenient configPath

countdown :: Word8 -> IO ()
countdown = CC.threadDelay . (* 1_000_000) . fromIntegral . (+ 1)
