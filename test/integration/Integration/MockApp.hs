{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides a mock Navi implementation.
module Integration.MockApp
  ( MockEnv (..),
    runMockApp,
    configToMockEnv,
  )
where

import Effectful.Dispatch.Dynamic (localSeqUnlift)
import Effectful.LoggerNS.Dynamic
  ( LoggerNSDynamic
      ( GetNamespace,
        LocalNamespace
      ),
  )
import Effectful.Reader.Static (runReader)
import Integration.Prelude
import Navi.Config (Config)
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
import Navi.Effectful.Notify (NotifyDynamic (SendNote))
import Navi.Effectful.Pythia (PythiaDynamic (Query))
import Navi.Env.Core
  ( HasEvents (getEvents),
    HasLogEnv (getLogEnv, localLogEnv),
    HasNoteQueue (getNoteQueue),
  )
import Navi.Event.Types (AnyEvent, EventError (MkEventError))
import Navi.Services.Types
  ( ServiceType
      ( BatteryPercentage,
        BatteryStatus,
        Multiple,
        NetworkInterface,
        Single
      ),
  )
import Numeric.Data.Interval (_MkLRInterval)
import Numeric.Data.Interval qualified as Interval
import Pythia.Control.Exception (CommandException (MkCommandException))
import Pythia.Services.Battery
  ( Battery (MkBattery),
    BatteryStatus (Charging, Discharging),
    Percentage (MkPercentage),
  )

-- | Mock configuration.
data MockEnv = MkMockEnv
  { events :: !(NonEmpty AnyEvent),
    logEnv :: !LogEnv,
    noteQueue :: !(TBQueue NaviNote),
    -- | "Sent" notifications are captured in this ref rather than
    -- actually sent. This way we can later test what was sent.
    sentNotes :: !(IORef [NaviNote]),
    -- | caches the last battery percentage "reading". This way we can
    -- ensure we have a new percentage every time.
    lastPercentage :: !(IORef Percentage)
  }

makeFieldLabelsNoPrefix ''MockEnv

instance HasEvents MockEnv where
  getEvents = view #events

instance HasLogEnv MockEnv where
  getLogEnv = view #logEnv
  localLogEnv _ = id

instance HasNoteQueue MockEnv where
  getNoteQueue = view #noteQueue

runLoggerMock :: Eff (LoggerDynamic : es) a -> Eff es a
runLoggerMock = interpret $ \_ -> \case
  LoggerLog {} -> pure ()

runLoggerNSMock :: Eff (LoggerNSDynamic : es) a -> Eff es a
runLoggerNSMock = interpret $ \env -> \case
  GetNamespace -> pure ""
  LocalNamespace _ m -> localSeqUnlift env $ \runner -> runner m

runNotifyMock ::
  ( IORefStatic :> es,
    Reader MockEnv :> es
  ) =>
  Eff (NotifyDynamic : es) a ->
  Eff es a
runNotifyMock = interpret $ \_ -> \case
  SendNote note -> do
    if note ^. #summary == "SentException"
      then throwM $ MkEventError "SentException" "sending mock exception" ""
      else do
        notes <- asks @MockEnv (view #sentNotes)
        modifyIORef' notes (note :)

runPythiaMock ::
  (IORefStatic :> es, Reader MockEnv :> es) =>
  Eff (PythiaDynamic : es) a ->
  Eff es a
runPythiaMock = interpret $ \_ -> \case
  -- Service that changes every time: can be used to test multiple
  -- notifications are sent.
  Query (BatteryPercentage _) -> do
    bpRef <- asks @MockEnv (view #lastPercentage)
    oldVal <- view (#unPercentage % _MkLRInterval) <$> readIORef bpRef
    let !newVal =
          if oldVal == 0
            then 100
            else oldVal - 1
        newBp = MkPercentage $ Interval.unsafeLRInterval newVal
    writeIORef bpRef newBp
    pure $ MkBattery newBp Discharging
  -- Constant service. Can test duplicate behavior.
  Query (BatteryStatus _) -> pure Charging
  -- Service error. Can test error behavior.
  Query (NetworkInterface _ _) ->
    throwM $ MkCommandException "nmcli" "Nmcli error"
  -- Constant service. Can test duplicate behavior.
  Query (Single _) -> pure "single trigger"
  -- Constant service. Can test duplicate behavior.
  Query (Multiple _) -> pure "multiple result"

runMockApp ::
  (IORefStatic :> es) =>
  Eff
    ( PythiaDynamic
        : NotifyDynamic
        : LoggerNSDynamic
        : LoggerDynamic
        : Reader MockEnv
        : es
    )
    a ->
  MockEnv ->
  Eff es a
runMockApp m env =
  runReader env
    . runLoggerMock
    . runLoggerNSMock
    . runNotifyMock
    . runPythiaMock
    $ m

configToMockEnv ::
  ( Concurrent :> es,
    IORefStatic :> es
  ) =>
  Config ->
  Eff es MockEnv
configToMockEnv config = do
  sentNotesRef <- newIORef []

  lastPercentageRef <- newIORef $ MkPercentage $ Interval.unsafeLRInterval 6
  logQueue <- newTBQueueA 1000
  noteQueue <- newTBQueueA 1000

  pure
    $ MkMockEnv
      { events = config ^. #events,
        sentNotes = sentNotesRef,
        logEnv =
          MkLogEnv
            { logLevel = LevelInfo,
              logHandle = Nothing,
              logNamespace = "",
              logQueue
            },
        noteQueue,
        lastPercentage = lastPercentageRef
      }
