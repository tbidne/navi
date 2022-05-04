-- | Entrypoint for integration tests. These test the behavior regarding
-- sending notifications/errors e.g. how notifications are sent depending
-- on config values (e.g. repeats, send errors).
--
-- In particular we do _not_ test:
-- - pythia integration (mocked)
-- - sending notifications (i.e. "sent" notifs are captured by our
--   MockApp rather than actually sent e.g. via dbus)
--
-- @since 0.1
module Main (main) where

import Control.Concurrent qualified as CC
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Integration.MockApp (MockEnv (..), runMockApp)
import Integration.Prelude
import Navi (runNavi)
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Data.NaviQueue (NaviQueue (..))
import Navi.Data.PollInterval (PollInterval (..))
import Navi.Event.Toml (ErrorNoteToml (..), RepeatEvtToml (..))
import Navi.Event.Types (AnyEvent)
import Navi.Services.Battery.Percentage qualified as Percentage
import Navi.Services.Battery.Percentage.Toml
  ( BatteryPercentageNoteToml (..),
    BatteryPercentageToml (..),
  )
import Navi.Services.Custom.Single qualified as Single
import Navi.Services.Custom.Single.Toml (SingleToml (..))
import Navi.Services.Network.NetInterfaces qualified as NetInterfaces
import Navi.Services.Network.NetInterfaces.Toml (NetInterfacesToml (..))
import Numeric.Data.Interval qualified as Interval
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Services.Battery (Percentage (..))
import Test.Tasty qualified as Tasty

-- | Runs integration tests.
main :: IO ()
main = do
  Tasty.defaultMain $
    Tasty.testGroup
      "Integration tests"
      [ testMultiNotifs,
        testDuplicates,
        testNoDuplicates,
        testNoDuplicateErrs,
        testSwallowErrs,
        testSendsMultipleErrs
      ]

testMultiNotifs :: TestTree
testMultiNotifs = testCase "Sends multiple new notifications" $ do
  percentageEvent <- mkPercentageEvent

  mockEnv <- mkEnv $ percentageEvent :| []
  runMock 6 mockEnv

  sentNotes <- readIORef $ mockEnv ^. #sentNotes
  5 @=? length sentNotes
  assertBool desc $ all ((==) "Battery Percentage" . view #summary) sentNotes
  where
    desc = "Summary should be 'Battery Percentage'"

testDuplicates :: TestTree
testDuplicates = testCase "Send duplicate notifications" $ do
  singleEvent <- mkSingleEvent (Just AllowRepeatsToml)

  mockEnv <- mkEnv $ singleEvent :| []
  runMock 3 mockEnv

  sentNotes <- readIORef $ mockEnv ^. #sentNotes
  assertBool "Should send at least 3 notifs" $ length sentNotes >= 3
  assertBool desc $ all ((==) "Single" . view #summary) sentNotes
  where
    desc = "Summary should be 'Single'"

testNoDuplicates :: TestTree
testNoDuplicates = testCase "Does not send duplicate notifications" $ do
  singleEvent <- mkSingleEvent Nothing

  mockEnv <- mkEnv $ singleEvent :| []
  runMock 3 mockEnv

  sentNotes <- readIORef $ mockEnv ^. #sentNotes
  1 @=? length sentNotes
  assertBool desc $ all ((==) "Single" . view #summary) sentNotes
  where
    desc = "Summary should be 'Single'"

testNoDuplicateErrs :: TestTree
testNoDuplicateErrs = testCase "Does not send duplicate errors" $ do
  -- Nothing = ErrNoteNoRepeatsToml i.e. No Repeats
  netInterfaceEvent <- mkNetInterfaceEvent Nothing

  mockEnv <- mkEnv $ netInterfaceEvent :| []
  runMock 3 mockEnv

  sentNotes <- readIORef $ mockEnv ^. #sentNotes
  1 @=? length sentNotes
  assertBool desc $ all ((==) "Exception" . view #summary) sentNotes
  where
    desc = "Summary should be 'Exception'"

testSwallowErrs :: TestTree
testSwallowErrs = testCase "Does not send any errors" $ do
  netInterfaceEvent <- mkNetInterfaceEvent (Just NoErrNoteToml)

  mockEnv <- mkEnv $ netInterfaceEvent :| []
  runMock 3 mockEnv

  sentNotes <- readIORef $ mockEnv ^. #sentNotes
  0 @=? length sentNotes

testSendsMultipleErrs :: TestTree
testSendsMultipleErrs = testCase "Sends multiple errors" $ do
  netInterfaceEvent <- mkNetInterfaceEvent (Just ErrNoteAllowRepeatsToml)

  mockEnv <- mkEnv $ netInterfaceEvent :| []
  runMock 3 mockEnv

  sentNotes <- readIORef $ mockEnv ^. #sentNotes
  assertBool "Should send at least 3 notifs" $ length sentNotes >= 3
  assertBool desc $ all ((==) "Exception" . view #summary) sentNotes
  where
    desc = "Summary should be 'Exception'"

runMock :: Word8 -> MockEnv -> IO ()
runMock maxSeconds env = do
  -- runNavi runs forever, so we use race_ to kill it once the countdown
  -- runs out.
  Async.race_ (countdown maxSeconds) (runMockApp runNavi env)

countdown :: Word8 -> IO ()
countdown 0 = pure ()
countdown !counter = CC.threadDelay 1_000_000 *> countdown (counter - 1)

mkEnv :: NonEmpty (AnyEvent IORef) -> IO MockEnv
mkEnv events = do
  sentNotesRef <- newIORef []

  lastPercentageRef <- newIORef $ MkPercentage $ Interval.unsafeLRInterval 6
  logQueue <- liftIO $ STM.atomically $ TBQueue.newTBQueue 1000
  noteQueue <- liftIO $ STM.atomically $ TBQueue.newTBQueue 1000

  let mockEnv =
        MkMockEnv
          { events = events,
            sentNotes = sentNotesRef,
            logQueue = MkNaviQueue logQueue,
            noteQueue = MkNaviQueue noteQueue,
            lastPercentage = lastPercentageRef
          }
  pure mockEnv

mkPercentageEvent :: IO (AnyEvent IORef)
mkPercentageEvent = do
  let toAlert i =
        MkBatteryPercentageNoteToml
          (MkPercentage (Interval.unsafeLRInterval i))
          Nothing
          Nothing
      alerts = toAlert <$> [1 .. 4]

      percentageToml =
        MkBatteryPercentageToml
          (toAlert 5 :| alerts)
          (Just $ MkPollInterval 1)
          Nothing
          Nothing
          Many

  Percentage.toEvent percentageToml

mkSingleEvent :: Maybe RepeatEvtToml -> IO (AnyEvent IORef)
mkSingleEvent repeatEventToml = do
  let note =
        MkNaviNote
          "Single"
          (Just "body")
          Nothing
          Nothing

      singleToml =
        MkSingleToml
          "cmd"
          "single trigger" -- matches trigger in MockApp
          (Just (MkPollInterval 1))
          note
          repeatEventToml
          Nothing

  Single.toEvent singleToml

mkNetInterfaceEvent :: Maybe ErrorNoteToml -> IO (AnyEvent IORef)
mkNetInterfaceEvent errNoteToml = do
  let netInterfacesToml =
        MkNetInterfacesToml
          Many
          "device"
          (Just (MkPollInterval 1))
          Nothing
          errNoteToml
          Nothing

  NetInterfaces.toEvent netInterfacesToml
