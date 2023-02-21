{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tests fatal exceptions.
module Integration.Exceptions (tests) where

import Control.Exception qualified as UnsafeEx
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (TimeOfDay), utc)
import Effects.Concurrent.Async (ExceptionInLinkedThread (..))
import Effects.Concurrent.Async qualified as Async
import Effects.Concurrent.Thread (sleep)
import Effects.Exception (ExceptionCS (..))
import Effects.LoggerNamespace
  ( MonadLoggerNamespace (..),
    defaultLogFormatter,
    formatLog,
  )
import Effects.System.Terminal (MonadTerminal (..))
import Effects.Time (MonadTime (..), ZonedTime (..))
import Integration.Prelude
import Navi (runNavi)
import Navi.Data.NaviLog (LogEnv (..))
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects.MonadNotify (MonadNotify (..))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (query))
import Navi.Env.Core (HasEvents (..), HasLogEnv (..), HasLogQueue (..), HasNoteQueue (..))
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    ErrorNote (..),
    Event (..),
    RepeatEvent (..),
  )
import Navi.NaviT (NaviT, runNaviT)
import Navi.Services.Types (ServiceType (..))
import Test.Tasty qualified as Tasty

data BadThread
  = LogThread
  | NotifyThread

-- | Mock configuration.
data ExceptionEnv = MkExceptionEnv
  { badThread :: !BadThread,
    events :: !(NonEmpty AnyEvent),
    logEnv :: !LogEnv,
    logQueue :: !(TBQueue LogStr),
    logsRef :: !(IORef (Seq ByteString)),
    noteQueue :: !(TBQueue NaviNote)
  }

makeFieldLabelsNoPrefix ''ExceptionEnv

instance HasEvents ExceptionEnv where
  getEvents = view #events

instance HasLogEnv ExceptionEnv where
  getLogEnv = view #logEnv
  localLogEnv = over' #logEnv

instance HasLogQueue ExceptionEnv where
  getLogQueue = view #logQueue

instance HasNoteQueue ExceptionEnv where
  getNoteQueue = view #noteQueue

newtype ExceptionIO a = MkExceptionIO (IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadAsync,
      MonadCatch,
      MonadHandleWriter,
      MonadIORef,
      MonadMask,
      MonadSTM,
      MonadThread,
      MonadThrow
    )
    via IO

newtype TestEx = MkTestE String
  deriving stock (Show)
  deriving anyclass (Exception)

instance MonadTerminal (NaviT ExceptionEnv ExceptionIO) where
  getChar = error "getChar: todo"
  getContents' = error "getContents': todo"
  getLine = error "getLine: todo"
  getTerminalSize = error "getTerminalSize: todo"

  -- NOTE: putBinary is used to fatally kill the logger thread, if we are
  -- testing it (badThread == LogThread)
  putBinary bs = do
    asks (view #badThread) >>= \case
      NotifyThread -> do
        logsRef <- asks (view #logsRef)
        modifyIORef' logsRef (bs :<|)
      LogThread -> sleep 2 *> throwM (MkTestE "logger dying")

instance MonadSystemInfo (NaviT ExceptionEnv ExceptionIO) where
  query = \case
    BatteryPercentage _ -> error "battery percentage unimplemented"
    BatteryStatus _ -> error "battery status unimplemented"
    NetworkInterface _ _ -> error "network interface unimplemented"
    Single _ -> pure "single"
    Multiple _ -> pure "multiple"

instance MonadLogger (NaviT ExceptionEnv ExceptionIO) where
  monadLoggerLog loc _src lvl msg = do
    logQueue <- asks getLogQueue
    logLevel <- asks (view #logLevel . getLogEnv)
    when (logLevel <= lvl) $ do
      formatted <- formatLog (defaultLogFormatter loc) lvl msg
      writeTBQueueM logQueue formatted

instance MonadLoggerNamespace (NaviT ExceptionEnv ExceptionIO) where
  getNamespace = asks (view #logNamespace . getLogEnv)
  localNamespace f = local (localLogEnv (over' #logNamespace f))

instance MonadTime (NaviT ExceptionEnv ExceptionIO) where
  getSystemZonedTime = pure zonedTime
  getMonotonicTime = pure 0

localTime :: LocalTime
localTime = LocalTime day tod
  where
    day = fromOrdinalDate 2022 39
    tod = TimeOfDay 10 20 5

zonedTime :: ZonedTime
zonedTime = ZonedTime localTime utc

instance MonadNotify (NaviT ExceptionEnv ExceptionIO) where
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
  "MkTestE \"logger dying\"" @=? displayException ex

badNotifierDies :: TestTree
badNotifierDies = testCase "Notify exception kills Navi" $ do
  (MkExceptionCS ex _, logs) <- runExceptionApp NotifyThread
  "MkTestE \"notify dying\"" @=? displayException @SomeException ex
  assertBool (show logs) $ errLog `elem` logs
  where
    errLog = "[2022-02-08 10:20:05][int-ex-test][Error][src/Navi.hs:110:8] Notify: MkTestE \"notify dying\"\n"

runExceptionApp ::
  forall e.
  (Exception e) =>
  BadThread ->
  IO (e, Seq ByteString)
runExceptionApp badThread = do
  let event =
        MkAnyEvent $
          MkEvent
            { name = "exception test",
              serviceType = Single "",
              pollInterval = 1,
              repeatEvent = AllowRepeats,
              errorNote = NoErrNote,
              raiseAlert = const Nothing
            }

  logQueue <- newTBQueueM 10
  noteQueue <- newTBQueueM 10
  logsRef <- newIORef []

  let env :: ExceptionEnv
      env =
        MkExceptionEnv
          { badThread,
            events = [event],
            logEnv =
              MkLogEnv
                { logFile = Nothing,
                  logLevel = LevelDebug,
                  logNamespace = "int-ex-test"
                },
            logQueue,
            logsRef,
            noteQueue
          }

      -- NOTE: timeout after 10 seconds
      MkExceptionIO testRun = Async.race (sleep 10_000_000) (runNaviT runNavi env)

  -- NOTE: The try that is in scope only works on sync exceptions, so we use
  -- the one from base.
  UnsafeEx.try @e testRun >>= \case
    Left ex -> do
      logs <- readIORef logsRef
      pure (ex, logs)
    Right (Left _) -> error "Exception test timed out!"
    Right (Right _) -> error "Navi finished successfully, impossible!"
