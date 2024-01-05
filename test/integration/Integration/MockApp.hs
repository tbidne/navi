{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides a mock Navi implementation.
module Integration.MockApp
  ( MockEnv (..),
    runMockApp,
    configToMockEnv,
  )
where

import Effects.LoggerNS (MonadLoggerNS (getNamespace, localNamespace))
import Effects.System.Terminal
  ( MonadTerminal
      ( getChar,
        getContents',
        getLine,
        getTerminalSize,
        putBinary,
        putStr,
        supportsPretty
      ),
  )
import Integration.Prelude
import Navi.Config (Config)
import Navi.Data.NaviLog (LogEnv (MkLogEnv))
import Navi.Data.NaviNote (NaviNote)
import Navi.Effects.MonadNotify (MonadNotify (sendNote))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (query))
import Navi.Env.Core
  ( HasEvents (getEvents),
    HasLogEnv (getLogEnv, localLogEnv),
    HasLogQueue (getLogQueue),
    HasNoteQueue (getNoteQueue),
  )
import Navi.Event.Types (AnyEvent, EventError (MkEventError))
import Navi.NaviT (NaviT, runNaviT)
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

-- | Mock configuration.
data MockEnv = MkMockEnv
  { events :: !(NonEmpty AnyEvent),
    logQueue :: !(TBQueue LogStr),
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
  getLogEnv = pure $ MkLogEnv Nothing LevelInfo ""
  localLogEnv _ = id

instance HasLogQueue MockEnv where
  getLogQueue = view #logQueue

instance HasNoteQueue MockEnv where
  getNoteQueue = view #noteQueue

newtype IntTestIO a = MkIntTestIO (IO a)
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
      MonadSTM,
      MonadTerminal,
      MonadThread,
      MonadThrow
    )
    via IO

makePrisms ''IntTestIO

instance MonadLogger (NaviT MockEnv IntTestIO) where
  -- if we ever decide to test logs, we can capture them similar to the
  -- MonadNotify instance.
  monadLoggerLog _loc _src _lvl _msg = pure ()

instance MonadTerminal (NaviT MockEnv IntTestIO) where
  getChar = liftIO getChar
  getLine = liftIO getLine
  getContents' = liftIO getContents'
  getTerminalSize = liftIO getTerminalSize
  putBinary = liftIO . putBinary
  putStr = liftIO . putStr
  putStrLn = liftIO . putStrLn
  supportsPretty = liftIO supportsPretty

instance MonadLoggerNS (NaviT MockEnv IntTestIO) where
  getNamespace = pure ""
  localNamespace _ = id

instance MonadNotify (NaviT MockEnv IntTestIO) where
  sendNote note =
    if note ^. #summary == "SentException"
      then throwM $ MkEventError "SentException" "sending mock exception" ""
      else do
        notes <- asks (view #sentNotes)
        liftIO $ modifyIORef' notes (note :)

instance MonadSystemInfo (NaviT MockEnv IntTestIO) where
  -- Service that changes every time: can be used to test multiple
  -- notifications are sent.
  query (BatteryPercentage _) = do
    bpRef <- asks (view #lastPercentage)
    oldVal <- liftIO $ Percentage.unPercentage <$> readIORef bpRef
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
  -- Constant service. Can test duplicate behavior.
  query (Single _) = pure "single trigger"
  -- Constant service. Can test duplicate behavior.
  query (Multiple _) = pure "multiple result"

runMockApp :: (NaviT MockEnv IntTestIO) a -> MockEnv -> IO a
runMockApp nt = view _MkIntTestIO . runNaviT nt

configToMockEnv :: Config -> IO MockEnv
configToMockEnv config = do
  sentNotesRef <- newIORef []

  lastPercentageRef <- newIORef $ Percentage.unsafePercentage 6
  logQueue <- newTBQueueA 1000
  noteQueue <- newTBQueueA 1000

  pure
    $ MkMockEnv
      { events = config ^. #events,
        sentNotes = sentNotesRef,
        logQueue = logQueue,
        noteQueue = noteQueue,
        lastPercentage = lastPercentageRef
      }
