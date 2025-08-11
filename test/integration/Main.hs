{-# LANGUAGE QuasiQuotes #-}

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

import DBus.Notify (UrgencyLevel (Critical))
import Data.Text qualified as T
import Effects.FileSystem.PathReader qualified as Dir
import Effects.FileSystem.PathWriter qualified as Dir
import Integration.Exceptions qualified as Exceptions
import Integration.MockApp (MockEnv, runMockApp)
import Integration.Prelude
import Navi.Data.NaviNote (NaviNote (MkNaviNote, body, summary, timeout, urgency))
import Navi.Event (EventError (MkEventError))
import Test.Tasty qualified as Tasty

-- | Runs integration tests.
main :: IO ()
main = do
  Tasty.defaultMain
    $ Tasty.testGroup
      "Integration tests"
      [ testMultiNotifs,
        testDuplicates,
        testNoDuplicates,
        testNoDuplicateErrs,
        testSwallowErrs,
        testSendsMultipleErrs,
        testSendExceptionDies,
        Exceptions.tests
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
    body = "Pythia exception: Command exception. Command: <nmcli>. Error: <Nmcli error>"

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
    body = "Pythia exception: Command exception. Command: <nmcli>. Error: <Nmcli error>"

testSendExceptionDies :: TestTree
testSendExceptionDies = testCase "Exception in send kills program" $ do
  result <-
    (runMock 3 sendExceptionConfig $> Nothing)
      `catch` (pure . Just)

  expected @=? result
  where
    expected = Just $ MkEventError "SentException" "sending mock exception" ""

runMock :: Word8 -> Text -> IO MockEnv
runMock maxSeconds config = do
  -- setup file
  tmp <- Dir.getTemporaryDirectory
  let configFp = tmp </> [osp|int.toml|]
  writeFileUtf8 configFp config'
  mockEnv <- runMockApp maxSeconds configFp
  Dir.removeFile configFp
  pure mockEnv
  where
    config' =
      T.unlines
        [ "[logging]",
          "location = \"stdout\"",
          config
        ]

batteryPercentageEventConfig :: Text
batteryPercentageEventConfig =
  T.unlines
    [ "[battery-percentage]",
      "app=\"sysfs\"",
      "poll-interval = 1",
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
      "poll-interval = 1",
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
      "app=\"nmcli\"",
      "poll-interval = 1",
      "device = \"device\"",
      errorEvents,
      ""
    ]

sendExceptionConfig :: Text
sendExceptionConfig =
  T.unlines
    [ "[[single]]",
      "poll-interval = 1",
      "name = \"Single\"",
      "command = \"cmd\"",
      "trigger = \"single trigger\"", -- needs to be triggered to be sent
      "",
      "[single.note]",
      "summary = \"SentException\"", -- matches MockApp's MonadNotify instance
      ""
    ]

mockEnvToNotes :: MockEnv -> IO [NaviNote]
mockEnvToNotes mockEnv = do
  sentNotes <- readIORef $ mockEnv ^. #sentNotes
  pure $ filter ((/= "Navi") . view #summary) sentNotes
