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
import DBus.Notify (UrgencyLevel (..))
import Data.Text qualified as T
import Integration.MockApp (MockEnv (..), configToMockEnv, runMockApp)
import Integration.Prelude
import Navi (runNavi)
import Navi.Config (readConfig)
import Navi.Data.NaviNote (NaviNote (..))
import System.Directory qualified as Dir
import System.FilePath ((</>))
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
  mockEnv <- runMock 6 batteryPercentageEventConfig

  sentNotes <- mockEnvToNotes mockEnv
  expected @=? sentNotes
  where
    expected = fmap toNote [1 .. 5 :: Int]
    toNote i =
      MkNaviNote
        { summary = "Battery Percentage",
          body = Just (showt i <> "%"),
          urgency = Nothing,
          timeout = Nothing
        }

testDuplicates :: TestTree
testDuplicates = testCase "Send duplicate notifications" $ do
  mockEnv <- runMock 3 (singleEventConfig "repeat-events = true")

  sentNotes <- mockEnvToNotes mockEnv
  expected @=? sentNotes
  where
    expected = replicate 4 $ MkNaviNote "Single" (Just "body") Nothing Nothing

testNoDuplicates :: TestTree
testNoDuplicates = testCase "Does not send duplicate notifications" $ do
  mockEnv <- runMock 3 (singleEventConfig "")

  sentNotes <- mockEnvToNotes mockEnv
  expected @=? sentNotes
  where
    expected = [MkNaviNote "Single" (Just "body") Nothing Nothing]

testNoDuplicateErrs :: TestTree
testNoDuplicateErrs = testCase "Does not send duplicate errors" $ do
  -- empty string = no duplicate errors
  mockEnv <- runMock 3 (netInterfaceEventConfig "")

  sentNotes <- mockEnvToNotes mockEnv
  expected @=? sentNotes
  where
    expected = [MkNaviNote "Exception" (Just body) (Just Critical) Nothing]
    body = "Command exception. Command: <nmcli>. Error: <Nmcli error>"

testSwallowErrs :: TestTree
testSwallowErrs = testCase "Does not send any errors" $ do
  mockEnv <- runMock 3 (netInterfaceEventConfig "error-events = \"none\"")

  sentNotes <- mockEnvToNotes mockEnv
  [] @=? sentNotes

testSendsMultipleErrs :: TestTree
testSendsMultipleErrs = testCase "Sends multiple errors" $ do
  mockEnv <- runMock 3 (netInterfaceEventConfig "error-events = \"repeats\"")

  sentNotes <- mockEnvToNotes mockEnv
  expected @=? sentNotes
  where
    expected = replicate 4 $ MkNaviNote "Exception" (Just body) (Just Critical) Nothing
    body = "Command exception. Command: <nmcli>. Error: <Nmcli error>"

runMock :: Word8 -> Text -> IO MockEnv
runMock maxSeconds config = do
  -- setup file
  tmp <- Dir.getTemporaryDirectory
  let configFp = tmp </> "int.toml"
  writeFileUtf8 configFp config
  -- file -> config
  cfg <- readConfig @IORef configFp
  mockEnv <- configToMockEnv cfg
  -- runNavi runs forever, so we use race_ to kill it once the countdown
  -- runs out.
  Async.race_ (countdown maxSeconds) (runMockApp runNavi mockEnv)
  Dir.removeFile configFp
  pure mockEnv

countdown :: Word8 -> IO ()
countdown 0 = pure ()
countdown !counter = CC.threadDelay 1_000_000 *> countdown (counter - 1)

batteryPercentageEventConfig :: Text
batteryPercentageEventConfig =
  T.unlines
    [ "[battery-percentage]",
      "poll-interval = \"1\"",
      "",
      "[[battery-percentage.alert]]",
      "percent = 5",
      "[[battery-percentage.alert]]",
      "percent = 4",
      "[[battery-percentage.alert]]",
      "percent = 3",
      "[[battery-percentage.alert]]",
      "percent = 2",
      "[[battery-percentage.alert]]",
      "percent = 1",
      ""
    ]

singleEventConfig :: Text -> Text
singleEventConfig repeats =
  T.unlines
    [ "[[single]]",
      "poll-interval = \"1\"",
      "command = \"cmd\"",
      "trigger = \"single trigger\"", -- matches trigger in MockApp
      repeats,
      "",
      "[single.note]",
      "summary = \"Single\"",
      "body = \"body\"",
      ""
    ]

netInterfaceEventConfig :: Text -> Text
netInterfaceEventConfig errorEvents =
  T.unlines
    [ "[[net-interface]]",
      "poll-interval = \"1\"",
      "device = \"device\"",
      errorEvents,
      ""
    ]

mockEnvToNotes :: MockEnv -> IO [NaviNote]
mockEnvToNotes mockEnv = do
  sentNotes <- readIORef $ mockEnv ^. #sentNotes
  pure $ filter ((/= "Navi") . view #summary) sentNotes
