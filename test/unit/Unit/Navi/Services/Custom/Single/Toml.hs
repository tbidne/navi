module Unit.Navi.Services.Custom.Single.Toml (tests) where

import DBus.Notify (UrgencyLevel (Critical))
import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote (..), Timeout (..))
import Navi.Event.Toml (ErrorNoteToml (..), RepeatEvtToml (..))
import Navi.Services.Custom.Single.Toml (SingleToml (..))
import Navi.Services.Custom.Single.Toml qualified as Single.Toml
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Services.Custom.Single.Toml"
    [ parsesCmd,
      parsesTrigger,
      repeatEventTests,
      errorNoteTests,
      parsesNote
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
          "timeout = \"5\""
        ]
    expected = MkNaviNote "a summary" (Just "a body") (Just Critical) (Just (Seconds 5))

repeatEventTests :: TestTree
repeatEventTests =
  testGroup
    "Parses repeat event"
    [ parsesRepeatEvent "off" "false" NoRepeatsToml,
      parsesRepeatEvent "on" "true" AllowRepeatsToml,
      parsesExpected "<none>" txt Nothing (view #repeatEvtCfg)
    ]
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "[note]",
          "summary = \"a summary\""
        ]

parsesRepeatEvent :: String -> Text -> RepeatEvtToml -> TestTree
parsesRepeatEvent desc flag ret =
  parsesExpected
    desc
    txt
    (Just ret)
    (view #repeatEvtCfg)
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
      parsesExpected "<none>" txt Nothing (view #errEvtCfg)
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
    (view #errEvtCfg)
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "trigger = \"a value\"",
          "error-events = \"" <> flag <> "\"",
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
parsesExpected = decodeExpected Single.Toml.singleCodec
