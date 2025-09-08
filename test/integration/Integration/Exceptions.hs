{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Tests fatal exceptions.
module Integration.Exceptions (tests) where

import Data.Text qualified as T
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime (LocalTime (LocalTime), TimeOfDay (TimeOfDay), utc)
import Effects.Concurrent.Async (ExceptionInLinkedThread (ExceptionInLinkedThread))
import Effects.Concurrent.Async qualified as Async
import Effects.Concurrent.Thread (sleep)
import Effects.FileSystem.FileReader (decodeUtf8Lenient)
import Effects.Logger.Namespace
  ( defaultLogFormatter,
    formatLog,
  )
import Effects.System.Terminal
  ( MonadTerminal
      ( putBinary
      ),
  )
import Effects.Time
  ( MonadTime (getMonotonicTime, getSystemZonedTime),
    ZonedTime (ZonedTime),
  )
import FileSystem.OsPath (decodeLenient)
import Integration.Prelude
import Navi (runNavi)
import Navi.Data.CommandResult (CommandResult (MkCommandResult))
import Navi.Effects.MonadNotify (MonadNotify (sendNote))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (query))
import Navi.Env.Core
  ( CoreEnvField (MkCoreEnvField),
    Env,
    HasEvents,
    HasLogEnv (getLogEnv),
    HasNoteQueue,
  )
import Navi.Runner qualified as Runner
import Navi.Services.Types
  ( ServiceType
      ( BatteryPercentage,
        BatteryStatus,
        Custom,
        NetworkInterface
      ),
  )
import Navi.Utils qualified as U
import System.Environment qualified as SysEnv
import System.IO (FilePath)
import Test.Tasty qualified as Tasty

data BadThread
  = LogThread
  | NotifyThread

-- | Mock configuration.
data ExceptionEnv = MkExceptionEnv
  { badThread :: BadThread,
    coreEnv :: Env,
    logsRef :: IORef (Seq ByteString)
  }

makeFieldLabelsNoPrefix ''ExceptionEnv

instance
  ( k ~ A_Lens,
    x ~ Namespace,
    y ~ Namespace
  ) =>
  LabelOptic "namespace" k ExceptionEnv ExceptionEnv x y
  where
  labelOptic =
    lensVL $ \f (MkExceptionEnv a1 a2 a3) ->
      fmap
        (\b -> MkExceptionEnv a1 (set' #namespace b a2) a3)
        (f (a2 ^. #namespace))
  {-# INLINE labelOptic #-}

deriving via (CoreEnvField ExceptionEnv) instance HasEvents ExceptionEnv

deriving via (CoreEnvField ExceptionEnv) instance HasLogEnv ExceptionEnv

deriving via (CoreEnvField ExceptionEnv) instance HasNoteQueue ExceptionEnv

newtype TestEx = MkTestE String
  deriving stock (Show)
  deriving anyclass (Exception)

newtype ExceptionsT a = MkExceptionsT (ReaderT ExceptionEnv IO a)
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
      MonadReader ExceptionEnv,
      MonadSTM,
      MonadThread,
      MonadThrow,
      MonadTypedProcess
    )
    via (ReaderT ExceptionEnv IO)

runExceptionsT :: ExceptionsT a -> ExceptionEnv -> IO a
runExceptionsT (MkExceptionsT rdr) = runReaderT rdr

instance MonadTerminal ExceptionsT where
  -- NOTE: putBinary is used to fatally kill the logger thread, if we are
  -- testing it (badThread == LogThread)
  putBinary bs = do
    asks (view #badThread) >>= \case
      NotifyThread -> do
        logsRef <- asks (view #logsRef)
        modifyIORef' logsRef (bs :<|)
      LogThread -> sleep 2 *> throwM (MkTestE "logger dying")

instance MonadSystemInfo ExceptionsT where
  query = \case
    BatteryPercentage _ -> error "battery percentage unimplemented"
    BatteryStatus _ -> error "battery status unimplemented"
    NetworkInterface _ _ -> error "network interface unimplemented"
    Custom _ _ -> pure (MkCommandResult Nothing Nothing "multiple", Nothing)

instance MonadLogger ExceptionsT where
  monadLoggerLog loc _src lvl msg = do
    mLogEnv <- asks getLogEnv
    case mLogEnv of
      Just logEnv -> do
        let logQueue = logEnv ^. #logQueue
            logLevel = logEnv ^. #logLevel
        when (logLevel <= lvl) $ do
          formatted <- formatLog (defaultLogFormatter loc) lvl msg
          writeTBQueueA logQueue formatted
      Nothing -> pure ()

instance MonadTime ExceptionsT where
  getSystemZonedTime = pure zonedTime
  getMonotonicTime = pure 0

localTime :: LocalTime
localTime = LocalTime day tod
  where
    day = fromOrdinalDate 2022 39
    tod = TimeOfDay 10 20 5

zonedTime :: ZonedTime
zonedTime = ZonedTime localTime utc

instance MonadNotify ExceptionsT where
  -- NOTE: sendNote is used to fatally kill the notify thread, if we are
  -- testing it (badThread == NotifyThread)
  sendNote _ = do
    asks (view #badThread) >>= \case
      LogThread -> pure ()
      NotifyThread -> sleep 2 *> throwM (MkTestE "notify dying")

-- | Runs integration tests.
tests :: TestTree
tests =
  Tasty.testGroup
    "Exceptions"
    [ badLoggerDies,
      badNotifierDies
    ]

badLoggerDies :: TestTree
badLoggerDies = testCase "Logger exception kills Navi" $ do
  (ExceptionInLinkedThread _ ex, _) <- runExceptionApp LogThread
  "MkTestE \"logger dying\"" @=? U.displayInner ex

badNotifierDies :: TestTree
badNotifierDies = testCase "Notify exception kills Navi" $ do
  (ex, logs) <- runExceptionApp @SomeException NotifyThread
  "MkTestE \"notify dying\"" @=? U.displayInner ex

  -- search for log
  foundLogRef <- newIORef False
  for_ logs $ \l -> do
    let t = decodeUtf8Lenient l
    when (errLog `T.isInfixOf` t) $ writeIORef foundLogRef True

  foundLog <- readIORef foundLogRef
  unless foundLog (assertFailure $ "Did not find expectedLog: " <> show logs)
  where
    errLog = "[Error] Notify: MkTestE \"notify dying\""

runExceptionApp ::
  forall e.
  (Exception e) =>
  BadThread ->
  IO (e, Seq ByteString)
runExceptionApp badThread = do
  logsRef <- newIORef []

  let action = SysEnv.withArgs args $ Runner.withEnv $ \coreEnv -> do
        let env =
              MkExceptionEnv
                { badThread,
                  coreEnv,
                  logsRef
                }
        Async.race
          (sleep 10)
          (runExceptionsT runNavi env)

  eResult <- try @_ @e action
  case eResult of
    Left ex -> do
      logs <- readIORef logsRef
      pure (ex, logs)
    Right (Left _) -> error "Exception test timed out!"
    Right (Right _) -> error "Navi finished successfully, impossible!"
  where
    args = ["-c", configPath]

configPath :: FilePath
configPath =
  decodeLenient
    $ [osp|test|]
    </> [osp|integration|]
    </> [osp|exceptions|]
    <> suffix
    <> [osp|.toml|]

{- ORMOLU_DISABLE -}

suffix :: OsPath
suffix =
#if OSX
  [osp|_osx|]
#else
  [osp|_linux|]
#endif

{- ORMOLU_ENABLE -}
