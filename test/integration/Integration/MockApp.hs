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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Effects.Concurrent.Async qualified as Async
import FileSystem.OsPath (decodeLenient)
import Integration.Prelude
import Navi (runNavi)
import Navi.Data.CommandResult (CommandResult)
import Navi.Data.CommandResultParser (CommandResultParser)
import Navi.Data.NaviNote (NaviNote)
import Navi.Data.PollInterval (PollInterval)
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
        Custom,
        NetworkInterface
      ),
  )
import Pythia.Control.Exception (CommandException (MkCommandException))
import Pythia.Data.Command (Command)
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
    customResponses :: IORef (Map Command [Text]),
    percentageResponses :: IORef [Percentage]
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
  -- Service that changes every time: can be used to test custom
  -- notifications are sent.
  query (BatteryPercentage _) = do
    responsesRef <- asks (view #percentageResponses)
    responses <- readIORef responsesRef
    newBp <- case responses of
      (r : rs) -> do
        writeIORef responsesRef rs
        pure r
      [] -> pure $ Percentage.unsafePercentage 80
    pure (MkBattery newBp Discharging, Nothing)
  -- Constant service. Can test duplicate behavior.
  query (BatteryStatus _) = pure (Charging, Nothing)
  -- Service error. Can test error behavior.
  query (NetworkInterface _ _) =
    throwM $ MkCommandException "nmcli" "Nmcli error"
  query (Custom cmd p) = getResponseOrDefault cmd p "custom result"

getResponseOrDefault ::
  Command ->
  CommandResultParser ->
  Text ->
  MockAppT (CommandResult, Maybe PollInterval)
getResponseOrDefault cmd parser def = do
  ref <- asks (view #customResponses)
  responseMap <- readIORef ref
  let responses = Map.findWithDefault [] cmd responseMap
  case responses of
    -- No events (i.e. using the default).
    [] -> parse def
    -- If we have 1 event left, just send it repeatedly.
    [r] -> parse r
    r : rs -> do
      writeIORef ref (Map.insert cmd rs responseMap)
      parse r
  where
    parse txt = case (parser ^. #unCommandResultParser) txt of
      Right x -> pure (x, x ^. #pollInterval)
      Left err -> error $ displayException err

runMockApp :: Word8 -> OsPath -> IO MockEnv
runMockApp = runMockAppEnv pure

runMockAppEnv :: (MockEnv -> IO MockEnv) -> Word8 -> OsPath -> IO MockEnv
runMockAppEnv modEnv maxSeconds configPath = do
  customResponses <- newIORef Map.empty
  percentageResponses <- newIORef []
  sentNotes <- newIORef []

  let action = SysEnv.withArgs args $ Runner.withEnv $ \coreEnv -> do
        let env =
              MkMockEnv
                { coreEnv,
                  sentNotes,
                  customResponses,
                  percentageResponses
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
