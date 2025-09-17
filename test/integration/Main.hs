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

import DBus.Notify (UrgencyLevel (Critical, Low))
import Data.List qualified as L
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Effects.FileSystem.PathReader qualified as Dir
import Effects.FileSystem.PathWriter qualified as Dir
import Integration.Exceptions qualified as Exceptions
import Integration.MockApp (MockEnv, runMockAppEnv)
import Integration.Prelude
import Navi.Data.NaviNote
  ( NaviNote
      ( MkNaviNote,
        body,
        summary,
        timeout,
        urgency
      ),
    Timeout (Seconds),
  )
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    EventError
      ( MkEventError,
        long,
        name,
        short
      ),
  )
import Navi.Services.Types (ServiceType (Custom), _Custom)
import Pythia.Data.Percentage (unsafePercentage)
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
        testMultipleRepeats,
        testMultipleCustomText,
        testBatteryPercentage,
        testDynamicPollIntervals,
        testOutputParensCommas,
        Exceptions.tests
      ]

testMultiNotifs :: TestTree
testMultiNotifs = testCase "Sends multiple new notifications" $ do
  mockEnv <- runMockEnv modEnv 6 batteryPercentageEventConfig

  sentNotes <- mockEnvToNotes mockEnv
  assertNotesOrder expected sentNotes
  where
    -- Percentage is counting down, hence we receive them in order 5 .. 1.
    expected = L.reverse $ fmap toNote [1 .. 5 :: Int]
    toNote i =
      MkNaviNote
        { summary = "Battery Percentage",
          body = Just (showt i <> "%"),
          urgency = Nothing,
          timeout = Nothing
        }

    modEnv :: MockEnv -> IO MockEnv
    modEnv env = do
      writeIORef
        (env ^. #percentageResponses)
        (unsafePercentage <$> [5, 4, 3, 2, 1])
      pure env

testDuplicates :: TestTree
testDuplicates = testCase "Send duplicate notifications" $ do
  mockEnv <- runMock 3 (singleEventConfig "repeat-events = true")

  sentNotes <- mockEnvToNotes mockEnv

  assertNoteRange 3 5 expected sentNotes
  where
    expected =
      MkNaviNote
        { summary = "Single",
          body = Just "body",
          urgency = Nothing,
          timeout = Nothing
        }

testNoDuplicates :: TestTree
testNoDuplicates = testCase "Does not send duplicate notifications" $ do
  mockEnv <- runMock 3 (singleEventConfig "")

  sentNotes <- mockEnvToNotes mockEnv
  expected @=? sentNotes
  where
    expected =
      [ MkNaviNote
          { summary = "Single",
            body = Just "body",
            urgency = Nothing,
            timeout = Nothing
          }
      ]

testNoDuplicateErrs :: TestTree
testNoDuplicateErrs = testCase "Does not send duplicate errors" $ do
  -- empty string = no duplicate errors
  mockEnv <- runMock 3 (netInterfaceEventConfig "")

  sentNotes <- mockEnvToNotes mockEnv
  expected @=? sentNotes
  where
    expected =
      [ MkNaviNote
          { summary = "Exception",
            body = Just body,
            urgency = Just Critical,
            timeout = Nothing
          }
      ]
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
    expected =
      MkNaviNote
        { summary = "Exception",
          body = Just body,
          urgency = Just Critical,
          timeout = Nothing
        }
    body = "Pythia exception: Command exception. Command: <nmcli>. Error: <Nmcli error>"

testSendExceptionDies :: TestTree
testSendExceptionDies = testCase "Exception in send kills program" $ do
  result <-
    (runMock 3 sendExceptionConfig $> Nothing)
      `catch` (pure . Just)

  expected @=? result
  where
    expected =
      Just
        $ MkEventError
          { name = "SentException",
            short = "sending mock exception",
            long = ""
          }

testReplaceText :: TestTree
testReplaceText = testCase "Replaces output text" $ do
  mockEnv <- runMockEnv modEnv 3 cfg

  sentNotes <- mockEnvToNotes mockEnv
  assertNotesRange 3 5 expected sentNotes
  where
    expected =
      Set.fromList
        [ MkNaviNote
            { summary = "Custom",
              body = Just "Result is o2",
              urgency = Nothing,
              timeout = Nothing
            },
          MkNaviNote
            { summary = "Custom",
              body = Just "Result is o1",
              urgency = Nothing,
              timeout = Nothing
            },
          MkNaviNote
            { summary = "Single",
              body = Just "result is: o1",
              urgency = Nothing,
              timeout = Nothing
            }
        ]

    modEnv :: MockEnv -> IO MockEnv
    modEnv env = do
      let mp =
            Map.fromList
              [ ("cmd1", ["(t1, o1)"]),
                ("cmd2", ["(t1, o1)", "(t2, o2)"])
              ]
      writeIORef (env ^. #customResponses) mp
      pure env

    cfg =
      T.unlines
        [ "[[custom]]",
          "poll-interval = 1",
          "command = \"cmd1\"",
          "command-result = \"(trigger, output)\"",
          "",
          "[[custom.trigger-note]]",
          "trigger = \"t1\"",
          "summary = \"Single\"",
          "body = \"result is: $out\"",
          "",
          "[[custom]]",
          "poll-interval = 1",
          "command = \"cmd2\"",
          "command-result = \"(trigger, output)\"",
          "",
          "[[custom.trigger-note]]",
          "trigger = \"t1\"",
          "summary = \"Custom\"",
          "body = \"Result is $out\"",
          "",
          "[[custom.trigger-note]]",
          "trigger = \"t2\"",
          "summary = \"Custom\"",
          "body = \"Result is $out\""
        ]

testMultipleRepeats :: TestTree
testMultipleRepeats = testCase "Uses multiple repeats" $ do
  mockEnv <- runMockEnv modEnv 6 cfg

  sentNotes <- mockEnvToNotes mockEnv
  expected @=? sentNotes
  where
    expected = t1s <> t2s

    t1s =
      replicate 2
        $ MkNaviNote
          { summary = "Custom",
            body = Just "Result is o1",
            urgency = Nothing,
            timeout = Nothing
          }

    t2s =
      replicate 3
        $ MkNaviNote
          { summary = "Custom",
            body = Just "Result is o2",
            urgency = Nothing,
            timeout = Nothing
          }

    modEnv :: MockEnv -> IO MockEnv
    modEnv env = do
      let t1 = "(t1, o1)"
          t2 = "(t2, o2)"
          t3 = "(t3, o3)"

          mp =
            Map.fromList
              [ ("cmd", [t1, t1, t3, t1, t2, t2, t2])
              ]

      -- Behavior should be:
      --
      -- t1 (sent)
      -- t1 (blocked)
      -- t3 (not raised)
      -- t1 (sent)
      -- t2 (sent)
      -- t2 (sent)
      -- t2 (sent)
      writeIORef (env ^. #customResponses) mp
      pure env

    cfg =
      T.unlines
        [ "[[custom]]",
          "poll-interval = 1",
          "command = \"cmd\"",
          "repeat-events = [\"t2\"]",
          "command-result = \"(trigger, output)\"",
          "",
          "[[custom.trigger-note]]",
          "trigger = \"t1\"",
          "summary = \"Custom\"",
          "body = \"Result is $out\"",
          "",
          "[[custom.trigger-note]]",
          "trigger = \"t2\"",
          "summary = \"Custom\"",
          "body = \"Result is $out\""
        ]

testMultipleCustomText :: TestTree
testMultipleCustomText = testCase "Tests custom dynamic example" $ do
  -- Tests complex config example that involves dynamic output and
  -- some repeat events.

  cfg <- modNoteSys <$> readFileUtf8ThrowM [ospPathSep|examples/config.toml|]

  mockEnv <- runMockNoConfigEnv modEnv 5 cfg

  sentNotes <- mockEnvToNotes mockEnv
  assertNotesRange 4 5 expected sentNotes
  where
    expected =
      Set.fromList
        [ MkNaviNote
            { summary = "Battery Percentage",
              body = Just "Battery is good: 70",
              urgency = Nothing,
              timeout = Just $ Seconds 10
            },
          MkNaviNote
            { summary = "Battery Percentage",
              body = Just "Battery is medium: 35",
              urgency = Nothing,
              timeout = Just $ Seconds 10
            },
          MkNaviNote
            { summary = "Battery Percentage",
              body = Just "Battery is low: 5",
              urgency = Just Critical,
              timeout = Just $ Seconds 10
            },
          MkNaviNote
            { summary = "Battery Percentage",
              body = Just "Battery is low: 4",
              urgency = Just Critical,
              timeout = Just $ Seconds 10
            }
        ]

    -- In addition to mocking the script responses, we want to filter out
    -- all the config examples we do not care about. We also need to set the
    -- poll-interval to something faster.
    modEnv :: MockEnv -> IO MockEnv
    modEnv env = do
      let t1 = "(good, 70)"
          t2 = "(good, 60)"
          t3 = "(med, 35)"
          t4 = "(med, 30)"
          t5 = "(low, 5)"
          t6 = "(low, 4)"
          mp =
            Map.fromList
              [ ("cmd", [t1, t2, t3, t4, t5, t6])
              ]

      writeIORef (env ^. #customResponses) mp

      let evts = case filteredEvts of
            [] -> error "empty list: "
            -- [e] -> NE.singleton e
            [MkAnyEvent e] -> case e ^. #serviceType of
              Custom _ _ ->
                NE.singleton
                  . MkAnyEvent
                  -- set cmn so mocked responses works
                  . set' (#serviceType % _Custom % _1) "cmd"
                  -- set to faster poll-interval
                  . set' #pollInterval 1
                  $ e
              other -> error $ "Expected custom serviceType, received: " ++ show other
            es@(_ : _ : _) -> error $ "Filter failed:" ++ show es

      pure
        . set' (#coreEnv % #events) evts
        $ env
      where
        takeExact (MkAnyEvent evt) = evt ^. #name == "battery-manual"

        filteredEvts =
          NE.filter takeExact
            $ env
            ^. (#coreEnv % #events)

testBatteryPercentage :: TestTree
testBatteryPercentage = testCase "Tests battery percentage example" $ do
  -- Tests complex config example that involves dynamic output and
  -- some repeat events.

  cfg <- modNoteSys <$> readFileUtf8ThrowM [ospPathSep|examples/config.toml|]

  mockEnv <- runMockNoConfigEnv modEnv 5 cfg

  sentNotes <- mockEnvToNotes mockEnv

  -- The total length of sentNotes is non-deterministic (can result in extra
  -- "2" messages), so we stop after verifying it has everything we want.
  assertNotesOrder expected sentNotes
  where
    expected =
      set' #urgency (Just Low) (mkNote "50%")
        : (mkNote <$> ["10%", "8%", "2%"])

    mkNote i =
      MkNaviNote
        { summary = "Battery Percentage",
          body = Just i,
          urgency = Just Critical,
          timeout = Nothing
        }

    modEnv :: MockEnv -> IO MockEnv
    modEnv env = do
      let ps = [50, 45, 15, 11, 10, 8, 2]
      writeIORef (env ^. #percentageResponses) (unsafePercentage <$> ps)

      let evts = case modEvts of
            [] -> error "empty list: "
            [e] -> NE.singleton e
            es@(_ : _ : _) -> error $ "Filter failed:" ++ show es

      pure
        . set' (#coreEnv % #events) evts
        $ env
      where
        takeExact (MkAnyEvent evt) = evt ^. #name == "battery-percentage"

        modEvts =
          fmap (\(MkAnyEvent e) -> MkAnyEvent (set' #pollInterval 1 e))
            . NE.filter takeExact
            $ env
            ^. (#coreEnv % #events)

testDynamicPollIntervals :: TestTree
testDynamicPollIntervals = testCase "Uses dynamic poll-interval" $ do
  mockEnv <- runMockEnv modEnv 5 cfg

  sentNotes <- mockEnvToNotes mockEnv
  assertNotesRange 6 6 expected sentNotes
  where
    expected =
      Set.fromList
        [ MkNaviNote
            { summary = "Single",
              body = Just "Result is one",
              urgency = Nothing,
              timeout = Nothing
            },
          MkNaviNote
            { summary = "Single",
              body = Just "Result is two",
              urgency = Nothing,
              timeout = Nothing
            },
          MkNaviNote
            { summary = "Single",
              body = Just "Result is three",
              urgency = Nothing,
              timeout = Nothing
            },
          MkNaviNote
            { summary = "Custom",
              body = Just "Result is one",
              urgency = Nothing,
              timeout = Nothing
            },
          MkNaviNote
            { summary = "Custom",
              body = Just "Result is two",
              urgency = Nothing,
              timeout = Nothing
            },
          MkNaviNote
            { summary = "Custom",
              body = Just "Result is three",
              urgency = Nothing,
              timeout = Nothing
            }
        ]

    modEnv :: MockEnv -> IO MockEnv
    modEnv env = do
      let mp =
            Map.fromList
              [ ("cmd1", ["(t1, one, 1)", "(t1, two, 1)", "(t1, three, 30)", "(t1, four, 30)"]),
                ("cmd2", ["(t1, 1, one)", "(t1, 1, two)", "(t2, 30, three)", "(t2, 30, four)"])
              ]

      writeIORef (env ^. #customResponses) mp
      pure env

    cfg =
      T.unlines
        [ "[[custom]]",
          "poll-interval = 40",
          "command = \"cmd1\"",
          "command-result = \"(trigger, output, poll-interval)\"",
          "repeat-events = true",
          "",
          "[[custom.trigger-note]]",
          "trigger = \"t1\"",
          "summary = \"Single\"",
          "body = \"Result is $out\"",
          "",
          "[[custom]]",
          "poll-interval = 40",
          "command = \"cmd2\"",
          "command-result = \"(trigger, poll-interval, output)\"",
          "repeat-events = [\"t1\", \"t2\"]",
          "",
          "[[custom.trigger-note]]",
          "trigger = \"t1\"",
          "summary = \"Custom\"",
          "body = \"Result is $out\"",
          "",
          "[[custom.trigger-note]]",
          "trigger = \"t2\"",
          "summary = \"Custom\"",
          "body = \"Result is $out\""
        ]

testOutputParensCommas :: TestTree
testOutputParensCommas = testCase "Custom output allows parens and commas" $ do
  mockEnv <- runMockEnv modEnv 2 cfg

  sentNotes <- mockEnvToNotes mockEnv
  assertNotesOrder expected sentNotes
  where
    expected = [n1, n2]

    n1 =
      MkNaviNote
        { summary = "Custom",
          body = Just "Result is (some, fancy, output)",
          urgency = Nothing,
          timeout = Nothing
        }

    n2 =
      MkNaviNote
        { summary = "Custom",
          body = Just "Result is (other, output)",
          urgency = Nothing,
          timeout = Nothing
        }

    modEnv :: MockEnv -> IO MockEnv
    modEnv env = do
      let t1 = "(t1, \"(some, fancy, output)\")"
          t2 = "(t2, \'(other, output)\')"

          mp =
            Map.fromList
              [ ("cmd", [t1, t2])
              ]

      -- Behavior should be:
      --
      -- t1 (sent)
      -- t2 (sent)
      writeIORef (env ^. #customResponses) mp
      pure env

    cfg =
      T.unlines
        [ "[[custom]]",
          "poll-interval = 1",
          "command = \"cmd\"",
          "repeat-events = [\"t2\"]",
          "command-result = \"(trigger, output)\"",
          "",
          "[[custom.trigger-note]]",
          "trigger = \"t1\"",
          "summary = \"Custom\"",
          "body = \"Result is $out\"",
          "",
          "[[custom.trigger-note]]",
          "trigger = \"t2\"",
          "summary = \"Custom\"",
          "body = \"Result is $out\""
        ]

-- Hacky, but we need to make some changes to examples/config.toml before
-- it is parsed into the Env.
modNoteSys :: Text -> Text
#if OSX
modNoteSys = T.replace "notify-send" "apple-script"
#else
modNoteSys = id
#endif

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

runMockNoConfigEnv :: (MockEnv -> IO MockEnv) -> Word8 -> Text -> IO MockEnv
runMockNoConfigEnv modEnv maxSeconds config = do
  -- setup file
  tmp <- Dir.getTemporaryDirectory
  let configFp = tmp </> [osp|int.toml|]
  writeFileUtf8 configFp config
  mockEnv <- runMockAppEnv modEnv maxSeconds configFp
  Dir.removeFile configFp
  pure mockEnv

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
    [ "[[custom]]",
      "poll-interval = 1",
      "command = \"cmd\"",
      repeats,
      "",
      "[[custom.trigger-note]]",
      "trigger = \"custom result\"", -- matches trigger in MockApp
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
    [ "[[custom]]",
      "poll-interval = 1",
      "name = \"Single\"",
      "command = \"cmd\"",
      "",
      "[[custom.trigger-note]]",
      "trigger = \"custom result\"", -- needs to be triggered to be sent
      "summary = \"SentException\"", -- matches MockApp's MonadNotify instance
      ""
    ]

mockEnvToNotes :: MockEnv -> IO [NaviNote]
mockEnvToNotes mockEnv = do
  sentNotes <- readIORef $ mockEnv ^. #sentNotes
  pure
    . L.reverse
    . filter ((/= "Navi") . view #summary)
    $ sentNotes

-- For when the number of received notest is non-deterministic
-- (i.e. based on timing).
assertNoteRange :: Int -> Int -> NaviNote -> [NaviNote] -> IO ()
assertNoteRange l r expected actual = do
  assertBool (show l ++ " <= " ++ show numActual) (l <= numActual)
  assertBool (show numActual ++ " <= " ++ show r) (numActual <= r)
  for_ actual $ \a -> expected @=? a
  where
    numActual = length actual

assertNotesOrder :: [NaviNote] -> [NaviNote] -> IO ()
assertNotesOrder expected actual =
  for_ (L.zip expected actual) $ uncurry (@=?)

-- The idea is we have some set of expected notes and non-deterministic
-- list of sent notes. We want to verify:
--
--   1. All expected were sent.
--   2. The sent notes fall within some numeric range.
--
-- Expected doesn't actually have to be a set, though it is mildly
-- convenient as it makes calling this function correctly easier.
assertNotesRange :: Int -> Int -> Set NaviNote -> [NaviNote] -> IO ()
assertNotesRange l r expected actual = do
  assertBool (mkErr $ showt l <> " <= " <> showt numActual) (l <= numActual)
  assertBool (mkErr $ showt numActual <> " <= " <> showt r) (numActual <= r)
  for_ expected $ \e -> do
    assertBool (mkErr $ "Did not found expected " <> showt e) (Set.member e actualSet)
  where
    numActual = length actual
    actualSet = Set.fromList actual

    mkErr msg =
      unpackText
        $ mconcat
          [ "Failed expectation '",
            msg,
            "': ",
            renderEvts
          ]

    renderEvts = mconcat $ fmap (("\n  - " <>) . showt) actual

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
