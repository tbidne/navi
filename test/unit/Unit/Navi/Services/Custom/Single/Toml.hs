module Unit.Navi.Services.Custom.Single.Toml (tests) where

import DBus.Notify (UrgencyLevel (Critical))
import Data.Text qualified as T
import Navi.Data.CommandResultParser
  ( CommandResultParserToml (MkCommandResultParserToml),
  )
import Navi.Data.NaviNote (NaviNote (MkNaviNote), Timeout (Seconds))
import Navi.Event.Toml
  ( ErrorNoteToml
      ( ErrNoteAllowRepeatsToml,
        ErrNoteNoRepeatsToml,
        NoErrNoteToml
      ),
    RepeatEventToml (AllowRepeatsToml, NoRepeatsToml),
  )
import Navi.Services.Custom.Single.Toml (SingleToml)
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Services.Custom.Single.Toml"
    [ parsesCmd,
      parsesTrigger,
      repeatEventTests,
      errorNoteTests,
      parsesNote,
      cmdResultTests
    ]

parsesCmd :: TestTree
parsesCmd =
  parsesExpected
    "Parses command"
    txt
    "  some multiline\n  command-1\n"
    (view #command)
  where
    txt =
      T.unlines
        [ "command = \"\"\"",
          "  some multiline",
          "  command-1",
          "\"\"\"",
          "trigger = \"a value\"",
          "[note]",
          "summary = \"a summary\""
        ]

parsesTrigger :: TestTree
parsesTrigger =
  parsesExpected
    "Parses trigger"
    txt
    "a value"
    (view #triggerVal)
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "[note]",
          "summary = \"a summary\""
        ]

parsesNote :: TestTree
parsesNote =
  parsesExpected
    "Parses note"
    txt
    expected
    (view #note)
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "[note]",
          "summary = \"a summary\"",
          "body = \"a body\"",
          "urgency = \"critical\"",
          "timeout = 5"
        ]
    expected = MkNaviNote "a summary" (Just "a body") (Just Critical) (Just (Seconds 5))

repeatEventTests :: TestTree
repeatEventTests =
  testGroup
    "Parses repeat event"
    [ parsesRepeatEvent "off" "false" NoRepeatsToml,
      parsesRepeatEvent "on" "true" AllowRepeatsToml,
      parsesExpected "<none>" txt Nothing (view #repeatEventCfg)
    ]
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "[note]",
          "summary = \"a summary\""
        ]

parsesRepeatEvent :: String -> Text -> RepeatEventToml -> TestTree
parsesRepeatEvent desc flag ret =
  parsesExpected
    desc
    txt
    (Just ret)
    (view #repeatEventCfg)
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "repeat-events = " <> flag,
          "[note]",
          "summary = \"a summary\""
        ]

errorNoteTests :: TestTree
errorNoteTests =
  testGroup
    "Parses error note"
    [ parsesErrorEvent "off" "none" NoErrNoteToml,
      parsesErrorEvent "on" "repeats" ErrNoteAllowRepeatsToml,
      parsesErrorEvent "on" "no-repeats" ErrNoteNoRepeatsToml,
      parsesExpected "<none>" txt Nothing (view #errEventCfg)
    ]
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "[note]",
          "summary = \"a summary\""
        ]

parsesErrorEvent :: String -> Text -> ErrorNoteToml -> TestTree
parsesErrorEvent desc flag ret =
  parsesExpected
    desc
    txt
    (Just ret)
    (view #errEventCfg)
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "error-events = \"" <> flag <> "\"",
          "[note]",
          "summary = \"a summary\""
        ]

cmdResultTests :: TestTree
cmdResultTests =
  testGroup
    "command-result"
    [ testCommandResultNone,
      cmdResult1Tests,
      cmdResult2Tests
    ]

testCommandResultNone :: TestTree
testCommandResultNone = assertDecode @SingleToml desc txt $ \toml -> do
  case toml ^. #parser of
    Nothing -> pure ()
    Just _ -> assertFailure "Expected no explicit parser"
  where
    desc = "No 'command-result' uses default parser"
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "[note]",
          "summary = \"a summary\""
        ]

cmdResult1Tests :: TestTree
cmdResult1Tests =
  testGroup
    "1 element"
    [ testCommandResult_T,
      testCommandResult_O,
      testCommandResultParens_T,
      testCommandResultParens_O
    ]

testCommandResult_T :: TestTree
testCommandResult_T = assertDecode @SingleToml desc txt $ \toml -> do
  case toml ^. #parser of
    Nothing -> assertFailure "Expected an explicit parser"
    Just (MkCommandResultParserToml p) -> do
      let parser = p "" ^. #unCommandResultParser
      -- always succeeds
      s1 <- assertRight $ parser "foo"
      "foo" @=? s1 ^. #result
      Nothing @=? s1 ^. #output
      Nothing @=? s1 ^. #pollInterval
  where
    desc = "'command-result = \"trigger\"' uses default parser"
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"trigger\"",
          "[note]",
          "summary = \"a summary\""
        ]

testCommandResult_O :: TestTree
testCommandResult_O = decodeExpectedFailure @SingleToml desc txt expected
  where
    desc = "'command-result = \"output\"' fails"
    expected = "Decode error at '.command-result': A single command result can only be 'trigger'."
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"output\"",
          "[note]",
          "summary = \"a summary\""
        ]

testCommandResultParens_T :: TestTree
testCommandResultParens_T = assertDecode @SingleToml desc txt $ \toml -> do
  case toml ^. #parser of
    Nothing -> assertFailure "Expected an explicit parser"
    Just (MkCommandResultParserToml p) -> do
      let parser = p "" ^. #unCommandResultParser
      -- always succeeds
      s1 <- assertRight $ parser "foo"
      "foo" @=? s1 ^. #result
      Nothing @=? s1 ^. #output
      Nothing @=? s1 ^. #pollInterval
  where
    desc = "'command-result = \"(trigger)\"' uses default parser"
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"(trigger)\"",
          "[note]",
          "summary = \"a summary\""
        ]

testCommandResultParens_O :: TestTree
testCommandResultParens_O = decodeExpectedFailure @SingleToml desc txt expected
  where
    desc = "'command-result = \"(output)\"' fails"
    expected = "Decode error at '.command-result': A single command result can only be 'trigger'."
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"(output)\"",
          "[note]",
          "summary = \"a summary\""
        ]

cmdResult2Tests :: TestTree
cmdResult2Tests =
  testGroup
    "2 elements"
    [ testCommandResult_T_T,
      testCommandResult_T_O,
      testCommandResult_O_T,
      testCommandResult_O_O
    ]

testCommandResult_T_T :: TestTree
testCommandResult_T_T = decodeExpectedFailure @SingleToml desc txt expected
  where
    desc = "'command-result = \"(trigger, trigger)\"' fails"
    expected = "Decode error at '.command-result': Found trigger twice, expected exactly one."
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"(trigger, trigger)\"",
          "[note]",
          "summary = \"a summary\""
        ]

testCommandResult_T_O :: TestTree
testCommandResult_T_O = assertDecode @SingleToml desc txt $ \toml -> do
  case toml ^. #parser of
    Nothing -> assertFailure "Expected an explicit parser"
    Just (MkCommandResultParserToml p) -> do
      let parser = p "name" ^. #unCommandResultParser
      e1 <- assertLeft $ parser "foo"
      "name: Parse error. Failed to parse (trigger, output): foo" @=? displayException e1

      e2 <- assertLeft $ parser "(foo)"
      "name: Parse error. Failed to parse (trigger, output): (foo)" @=? displayException e2

      s1 <- assertRight $ parser "(some result, some output)"
      "some result" @=? s1 ^. #result
      Just "some output" @=? s1 ^. #output
      Nothing @=? s1 ^. #pollInterval
  where
    desc = "'command-result = \"(trigger, output)\"'"
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"(trigger, output)\"",
          "[note]",
          "summary = \"a summary\""
        ]

testCommandResult_O_T :: TestTree
testCommandResult_O_T = assertDecode @SingleToml desc txt $ \toml -> do
  case toml ^. #parser of
    Nothing -> assertFailure "Expected an explicit parser"
    Just (MkCommandResultParserToml p) -> do
      let parser = p "name" ^. #unCommandResultParser
      e1 <- assertLeft $ parser "foo"
      "name: Parse error. Failed to parse (output, trigger): foo" @=? displayException e1

      e2 <- assertLeft $ parser "(foo)"
      "name: Parse error. Failed to parse (output, trigger): (foo)" @=? displayException e2

      s1 <- assertRight $ parser "(some output, some result)"
      "some result" @=? s1 ^. #result
      Just "some output" @=? s1 ^. #output
      Nothing @=? s1 ^. #pollInterval
  where
    desc = "''command-result = \"(output, trigger)\"'"
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"(output, trigger)\"",
          "[note]",
          "summary = \"a summary\""
        ]

testCommandResult_O_O :: TestTree
testCommandResult_O_O = decodeExpectedFailure @SingleToml desc txt expected
  where
    desc = "'command-result = \"(output, output)\"' fails"
    expected = "Decode error at '.command-result': Found output twice, expected at most one."
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"(output, output)\"",
          "[note]",
          "summary = \"a summary\""
        ]

parsesExpected ::
  (Eq a, Show a) =>
  String ->
  Text ->
  a ->
  (SingleToml -> a) ->
  TestTree
parsesExpected = decodeExpected
