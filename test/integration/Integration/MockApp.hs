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
import Navi.Event.Types (EventError (MkEventError, long, name, short))
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
    customResponses :: TVar (Map Command [Text]),
    percentageResponses :: TVar [Percentage],
    -- | "Sent" notifications are captured in this ref rather than
    -- actually sent. This way we can later test what was sent.
    sentNotes :: TVar [NaviNote]
  }

instance
  (k ~ A_Lens, a ~ Env, b ~ Env) =>
  LabelOptic "coreEnv" k MockEnv MockEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkMockEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkMockEnv b a2 a3 a4)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TVar (Map Command [Text]), b ~ TVar (Map Command [Text])) =>
  LabelOptic "customResponses" k MockEnv MockEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkMockEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkMockEnv a1 b a3 a4)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TVar [Percentage], b ~ TVar [Percentage]) =>
  LabelOptic "percentageResponses" k MockEnv MockEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkMockEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkMockEnv a1 a2 b a4)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ TVar [NaviNote], b ~ TVar [NaviNote]) =>
  LabelOptic "sentNotes" k MockEnv MockEnv a b
  where
  labelOptic =
    lensVL
      $ \f (MkMockEnv a1 a2 a3 a4) ->
        fmap
          (\b -> MkMockEnv a1 a2 a3 b)
          (f a4)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, x ~ Namespace, y ~ Namespace) =>
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
      then
        throwM
          $ MkEventError
            { name = "SentException",
              short = "sending mock exception",
              long = ""
            }
      else do
        notes <- asks (view #sentNotes)
        liftIO $ modifyTVarA' notes (note :)

instance MonadSystemInfo MockAppT where
  -- Service that changes every time: can be used to test custom
  -- notifications are sent.
  query (BatteryPercentage _) = do
    responsesRef <- asks (view #percentageResponses)

    newBp <- atomically $ do
      responses <- readTVar responsesRef
      case responses of
        r : rs -> do
          writeTVar responsesRef rs
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

  -- We have an apparent race condition that is difficult to reproduce.
  -- We sometimes get repeated events that _shouldn't_ happen, but probably
  -- occur due to these variables previously being IORef.
  --
  -- Hence we switch the IORefs to TVars and surround read/write logic
  -- with atomically.
  result <- atomically $ do
    responseMap <- readTVar ref
    let responses = Map.findWithDefault [] cmd responseMap
    case responses of
      -- No events (i.e. using the default).
      [] -> pure def
      -- If we have 1 event left, just send it repeatedly.
      [r] -> pure r
      r : rs -> do
        writeTVar ref (Map.insert cmd rs responseMap)
        pure r

  parse result
  where
    parse txt = case (parser ^. #unCommandResultParser) txt of
      Right x -> pure (x, x ^. #pollInterval)
      Left err -> error $ displayException err

runMockApp :: Word8 -> OsPath -> IO MockEnv
runMockApp = runMockAppEnv pure

runMockAppEnv :: (MockEnv -> IO MockEnv) -> Word8 -> OsPath -> IO MockEnv
runMockAppEnv modEnv maxSeconds configPath = do
  customResponses <- newTVarA Map.empty
  percentageResponses <- newTVarA []
  sentNotes <- newTVarA []

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
