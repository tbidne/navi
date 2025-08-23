module Unit.Navi.Services.Custom.Multiple.Toml (tests) where

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
    MultiRepeatEventToml (MultiAllowRepeatsToml, MultiNoRepeatsToml),
  )
import Navi.Services.Custom.Multiple.Toml
  ( MultipleToml,
    TriggerNoteToml
      ( MkTriggerNoteToml,
        note,
        trigger
      ),
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Services.Custom.Multiple.Toml"
    [ parsesCmd,
      parsesNotes,
      repeatEventTests,
      errorNoteTests,
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
          "[[trigger-note]]",
          "trigger = \"value\"",
          "summary = \"a summary\""
        ]

parsesNotes :: TestTree
parsesNotes =
  parsesExpected
    "Parses notes"
    txt
    (firstNote :| [secondNote])
    (view #triggerNotes)
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "[[trigger-note]]",
          "trigger = \"first val\"",
          "summary = \"first summary\"",
          "body = \"first body\"",
          "urgency = \"critical\"",
          "timeout = 5",
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "summary = \"second summary\""
        ]
    firstNote =
      MkTriggerNoteToml
        { trigger = "first val",
          note = MkNaviNote "first summary" (Just "first body") (Just Critical) (Just $ Seconds 5)
        }
    secondNote =
      MkTriggerNoteToml
        { trigger = "second val",
          note = MkNaviNote "second summary" Nothing Nothing Nothing
        }

repeatEventTests :: TestTree
repeatEventTests =
  testGroup
    "Parses repeat event"
    [ parsesRepeatEvent "off" "false" MultiNoRepeatsToml,
      parsesRepeatEvent "on" "true" MultiAllowRepeatsToml,
      parsesExpected "<none>" txt Nothing (view #repeatEventCfg)
    ]
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "[[trigger-note]]",
          "trigger = \"value\"",
          "summary = \"a summary\""
        ]

parsesRepeatEvent :: String -> Text -> MultiRepeatEventToml Text -> TestTree
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
          "repeat-events = " <> flag,
          "[[trigger-note]]",
          "trigger = \"value\"",
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
          "[[trigger-note]]",
          "trigger = \"value\"",
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
          "error-events = \"" <> flag <> "\"",
          "[[trigger-note]]",
          "trigger = \"value\"",
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
testCommandResultNone = assertDecode @MultipleToml desc txt $ \toml -> do
  case toml ^. #parser of
    Nothing -> pure ()
    Just _ -> assertFailure "Expected no explicit parser"
  where
    desc = "No 'command-result' uses default parser"
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "summary = \"second summary\""
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
testCommandResult_T = assertDecode @MultipleToml desc txt $ \toml -> do
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
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "summary = \"second summary\""
        ]

testCommandResult_O :: TestTree
testCommandResult_O = decodeExpectedFailure @MultipleToml desc txt expected
  where
    desc = "'command-result = \"output\"' fails"
    expected = "Decode error at '.command-result': A single command result can only be 'trigger'."
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"output\"",
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "summary = \"second summary\""
        ]

testCommandResultParens_T :: TestTree
testCommandResultParens_T = assertDecode @MultipleToml desc txt $ \toml -> do
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
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "summary = \"second summary\""
        ]

testCommandResultParens_O :: TestTree
testCommandResultParens_O = decodeExpectedFailure @MultipleToml desc txt expected
  where
    desc = "'command-result = \"(output)\"' fails"
    expected = "Decode error at '.command-result': A single command result can only be 'trigger'."
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"(output)\"",
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "summary = \"second summary\""
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
testCommandResult_T_T = decodeExpectedFailure @MultipleToml desc txt expected
  where
    desc = "'command-result = \"(trigger, trigger)\"' fails"
    expected = "Decode error at '.command-result': Found trigger twice, expected exactly one."
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"(trigger, trigger)\"",
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "summary = \"second summary\""
        ]

testCommandResult_T_O :: TestTree
testCommandResult_T_O = assertDecode @MultipleToml desc txt $ \toml -> do
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
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "summary = \"second summary\""
        ]

testCommandResult_O_T :: TestTree
testCommandResult_O_T = assertDecode @MultipleToml desc txt $ \toml -> do
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
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "summary = \"second summary\""
        ]

testCommandResult_O_O :: TestTree
testCommandResult_O_O = decodeExpectedFailure @MultipleToml desc txt expected
  where
    desc = "'command-result = \"(output, output)\"' fails"
    expected = "Decode error at '.command-result': Found output twice, expected at most one."
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "command-result = \"(output, output)\"",
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "summary = \"second summary\""
        ]

parsesExpected ::
  (Eq a, Show a) =>
  String ->
  Text ->
  a ->
  (MultipleToml -> a) ->
  TestTree
parsesExpected = decodeExpected
