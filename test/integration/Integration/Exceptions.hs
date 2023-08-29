{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Tests fatal exceptions.
module Integration.Exceptions (tests) where

import Control.Exception qualified as UnsafeEx
import Data.Foldable (toList)
import Data.IORef qualified as IORef
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Seq
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.Time.LocalTime (LocalTime (LocalTime), TimeOfDay (TimeOfDay), utc)
import Effectful.Concurrent (runConcurrent)
import Effectful.Concurrent.Async
  ( ExceptionInLinkedThread (ExceptionInLinkedThread),
  )
import Effectful.Concurrent.Async qualified as Async
import Effectful.Concurrent.Static (sleep)
import Effectful.FileSystem.HandleWriter.Dynamic (runHandleWriterDynamicIO)
import Effectful.IORef.Static (runIORefStaticIO)
import Effectful.LoggerNS.Dynamic
  ( LocStrategy (LocStable),
    defaultLogFormatter,
    formatLog,
  )
import Effectful.Reader.Static (runReader)
import Effectful.Terminal.Dynamic (TerminalDynamic (PutBinary))
import Effectful.Time.Dynamic
  ( TimeDynamic
      ( GetMonotonicTime,
        GetSystemZonedTime
      ),
    ZonedTime (ZonedTime),
  )
import Integration.Prelude
import Navi (runNavi)
import Navi.Data.NaviLog
  ( LogEnv
      ( MkLogEnv,
        logHandle,
        logLevel,
        logNamespace,
        logQueue
      ),
  )
import Navi.Data.NaviNote (NaviNote)
import Navi.Effectful.Logging (runLoggerDynamicNS)
import Navi.Effectful.Notify (NotifyDynamic (SendNote))
import Navi.Effectful.Pythia (PythiaDynamic (Query))
import Navi.Env.Core
  ( HasEvents (getEvents),
    HasLogEnv (getLogEnv, localLogEnv),
    HasNoteQueue (getNoteQueue),
  )
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    ErrorNote (NoErrNote),
    Event
      ( MkEvent,
        errorNote,
        name,
        pollInterval,
        raiseAlert,
        repeatEvent,
        serviceType
      ),
    RepeatEvent (AllowRepeats),
  )
import Navi.Services.Types
  ( ServiceType
      ( BatteryPercentage,
        BatteryStatus,
        Multiple,
        NetworkInterface,
        Single
      ),
  )
import Test.Tasty qualified as Tasty

data BadThread
  = LogThread
  | NotifyThread

-- | Mock configuration.
data ExceptionEnv = MkExceptionEnv
  { badThread :: !BadThread,
    events :: !(NonEmpty AnyEvent),
    logEnv :: !LogEnv,
    logsRef :: !(IORef (Seq ByteString)),
    noteQueue :: !(TBQueue NaviNote)
  }

makeFieldLabelsNoPrefix ''ExceptionEnv

instance HasEvents ExceptionEnv where
  getEvents = view #events

instance HasLogEnv ExceptionEnv where
  getLogEnv = view #logEnv
  localLogEnv = over' #logEnv

instance HasNoteQueue ExceptionEnv where
  getNoteQueue = view #noteQueue

newtype TestEx = MkTestE String
  deriving stock (Show)
  deriving anyclass (Exception)

runTerminalMock ::
  ( Concurrent :> es,
    IORefStatic :> es,
    Reader ExceptionEnv :> es
  ) =>
  Eff (TerminalDynamic : es) a ->
  Eff es a
runTerminalMock = interpret $ \_ -> \case
  PutBinary bs -> do
    asks @ExceptionEnv (view #badThread) >>= \case
      NotifyThread -> do
        logsRef <- asks @ExceptionEnv (view #logsRef)
        modifyIORef' logsRef (bs :<|)
      LogThread -> sleep 2 *> throwM (MkTestE "logger dying")
  _ -> error "todo"

runPythiaMock :: Eff (PythiaDynamic : es) a -> Eff es a
runPythiaMock = interpret $ \_ -> \case
  Query (BatteryPercentage _) -> error "battery percentage unimplemented"
  Query (BatteryStatus _) -> error "battery status unimplemented"
  Query (NetworkInterface _ _) -> error "network interface unimplemented"
  Query (Single _) -> pure "single"
  Query (Multiple _) -> pure "multiple"

runLoggerDynamicMock ::
  forall es a.
  ( Concurrent :> es,
    LoggerNSDynamic :> es,
    Reader ExceptionEnv :> es,
    TimeDynamic :> es
  ) =>
  Eff (LoggerDynamic : es) a ->
  Eff es a
runLoggerDynamicMock = interpret $ \_ -> \case
  LoggerLog loc _src lvl msg -> do
    logEnv <- asks @ExceptionEnv getLogEnv
    let logQueue = view #logQueue logEnv
        logLevel = view #logLevel logEnv
    when (logLevel <= lvl) $ do
      let fmt = set' #locStrategy (LocStable loc) (defaultLogFormatter loc)
      formatted <- formatLog fmt lvl msg
      writeTBQueueA logQueue formatted

runTimeDynamicMock :: Eff (TimeDynamic : es) a -> Eff es a
runTimeDynamicMock = interpret $ \_ -> \case
  GetSystemZonedTime -> pure zonedTime
  GetMonotonicTime -> pure 0

localTime :: LocalTime
localTime = LocalTime day tod
  where
    day = fromOrdinalDate 2022 39
    tod = TimeOfDay 10 20 5

zonedTime :: ZonedTime
zonedTime = ZonedTime localTime utc

runNotifyMock ::
  ( Concurrent :> es,
    Reader ExceptionEnv :> es
  ) =>
  Eff (NotifyDynamic : es) a ->
  Eff es a
runNotifyMock = interpret $ \_ -> \case
  SendNote _ -> do
    -- NOTE: sendNote is used to fatally kill the notify thread, if we are
    -- testing it (badThread == NotifyThread)
    asks @ExceptionEnv (view #badThread) >>= \case
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
  (ex, logs) <- runExceptionApp NotifyThread
  "MkTestE \"notify dying\"" @=? displayException @SomeException ex
  assertBool (prettyLogs logs) $ errLog `elem` logs
  where
    errLog = "[2022-02-08 10:20:05][int-ex-test][Error][src/Navi.hs] Notify: MkTestE \"notify dying\"\n"

    prettyLogs =
      L.unlines
        . fmap show
        . toList

runExceptionApp ::
  forall e.
  (Exception e) =>
  BadThread ->
  IO (e, Seq ByteString)
runExceptionApp badThread = do
  let event =
        MkAnyEvent
          $ MkEvent
            { name = "exception test",
              serviceType = Single "",
              pollInterval = 1,
              repeatEvent = AllowRepeats,
              errorNote = NoErrNote,
              raiseAlert = const Nothing
            }

  logQueue <- runSTM $ newTBQueueA 10
  noteQueue <- runSTM $ newTBQueueA 10
  logsRef <- IORef.newIORef Seq.empty

  let env :: ExceptionEnv
      env =
        MkExceptionEnv
          { badThread,
            events = NE.singleton event,
            logEnv =
              MkLogEnv
                { logHandle = Nothing,
                  logLevel = LevelDebug,
                  logQueue,
                  logNamespace = "int-ex-test"
                },
            logsRef,
            noteQueue
          }

      -- NOTE: timeout after 10 seconds
      -- MkExceptionIO testRun = Async.race (sleep 10_000_000) (runNaviT runNavi env)
      testRun =
        runEff
          . runConcurrent
          $ Async.race
            (sleep 10_000_000)
            ( runReader env
                . runHandleWriterDynamicIO
                . runIORefStaticIO
                . runNotifyMock
                . runPythiaMock
                . runTerminalMock
                . runTimeDynamicMock
                . runLoggerDynamicNS @ExceptionEnv
                . runLoggerDynamicMock
                $ runNavi @ExceptionEnv
            )

  -- NOTE: The try that is in scope only works on sync exceptions, so we use
  -- the one from base.
  UnsafeEx.try @e testRun >>= \case
    Left ex -> do
      logs <- IORef.readIORef logsRef
      pure (ex, logs)
    Right (Left _) -> error "Exception test timed out!"
    Right (Right _) -> error "Navi finished successfully, impossible!"

runSTM :: Eff [Concurrent, IOE] a -> IO a
runSTM = runEff . runConcurrent
