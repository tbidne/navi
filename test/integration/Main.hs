{-# LANGUAGE CPP #-}
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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effects.FileSystem.PathReader qualified as Dir
import Effects.FileSystem.PathWriter qualified as Dir
import Integration.Exceptions qualified as Exceptions
import Integration.MockApp (MockEnv, runMockAppEnv)
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
        testReplaceText,
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
  assertNoteRange 3 5 expected sentNotes
  where
    expected = MkNaviNote "Single" (Just "body") Nothing Nothing

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
  assertNoteRange 3 5 expected sentNotes
  where
    expected = MkNaviNote "Exception" (Just body) (Just Critical) Nothing
    body = "Pythia exception: Command exception. Command: <nmcli>. Error: <Nmcli error>"

testSendExceptionDies :: TestTree
testSendExceptionDies = testCase "Exception in send kills program" $ do
  result <-
    (runMock 3 sendExceptionConfig $> Nothing)
      `catch` (pure . Just)

  expected @=? result
  where
    expected = Just $ MkEventError "SentException" "sending mock exception" ""

testReplaceText :: TestTree
testReplaceText = testCase "Replaces trigger text" $ do
  mockEnv <- runMockEnv modEnv 3 cfg

  sentNotes <- mockEnvToNotes mockEnv
  assertNotes expected sentNotes
  where
    expected =
      Set.fromList
        [ MkNaviNote
            { summary = "Multiple",
              body = Just "Result is t2",
              urgency = Nothing,
              timeout = Nothing
            },
          MkNaviNote
            { summary = "Multiple",
              body = Just "Result is t1",
              urgency = Nothing,
              timeout = Nothing
            },
          MkNaviNote
            { summary = "Single",
              body = Just "result is: single trigger",
              urgency = Nothing,
              timeout = Nothing
            }
        ]

    modEnv :: MockEnv -> IO MockEnv
    modEnv env = do
      writeIORef (env ^. #multipleResponses) ["t1", "t2"]
      pure env

    cfg =
      T.unlines
        [ "[[single]]",
          "poll-interval = 1",
          "command = \"cmd\"",
          "trigger = \"single trigger\"",
          "",
          "[single.note]",
          "summary = \"Single\"",
          "body = \"result is: $trigger\"",
          "",
          "[[multiple]]",
          "poll-interval = 1",
          "command = \"cmd\"",
          "",
          "[[multiple.trigger-note]]",
          "trigger = \"t1\"",
          "summary = \"Multiple\"",
          "body = \"Result is $trigger\"",
          "",
          "[[multiple.trigger-note]]",
          "trigger = \"t2\"",
          "summary = \"Multiple\"",
          "body = \"Result is $trigger\""
        ]

runMock :: Word8 -> Text -> IO MockEnv
runMock = runMockEnv pure

runMockEnv :: (MockEnv -> IO MockEnv) -> Word8 -> Text -> IO MockEnv
runMockEnv modEnv maxSeconds config = do
  -- setup file
  tmp <- Dir.getTemporaryDirectory
  let configFp = tmp </> [osp|int.toml|]
  writeFileUtf8 configFp config'
  mockEnv <- runMockAppEnv modEnv maxSeconds configFp
  Dir.removeFile configFp
  pure mockEnv
  where
    config' =
      T.unlines
        [ headerConfig,
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

-- For when the number of received notest is non-deterministic
-- (i.e. based on timing).
assertNoteRange :: Int -> Int -> NaviNote -> [NaviNote] -> IO ()
assertNoteRange l r expected actual = do
  assertBool (show l ++ " <= " ++ show numActual) (l <= numActual)
  assertBool (show numActual ++ " <= " ++ show r) (numActual <= r)
  for_ actual $ \a -> expected @=? a
  where
    numActual = length actual

assertNotes :: Set NaviNote -> [NaviNote] -> IO ()
assertNotes expected actual = do
  length expected @=? length actual
  for_ actual $ \a -> assertBool (show a) (Set.member a expected)

headerConfig :: Text
headerConfig =
  T.unlines
    [ notifyConfig,
      "[logging]",
      "severity = \"none\""
    ]

notifyConfig :: Text
notifyConfig =
  mconcat
    [ "note-system = \"",
      notifySystem,
      "\""
    ]

{- ORMOLU_DISABLE -}

notifySystem :: Text
notifySystem =
#if OSX
  "apple-script"
#else
  "notify-send"
#endif

{- ORMOLU_ENABLE -}
