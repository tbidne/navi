module Unit.Navi.Services.Custom.Multiple.Toml (tests) where

import DBus.Notify (UrgencyLevel (Critical))
import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote (..), Timeout (..))
import Navi.Event.Toml (ErrorNoteToml (..), RepeatEventToml (..))
import Navi.Services.Custom.Multiple.Toml
  ( MultipleToml (..),
    TriggerNoteToml (..),
    multipleCodec,
  )
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "Navi.Services.Custom.Multiple.Toml"
    [ parsesCmd,
      parsesNotes,
      repeatEventTests,
      errorNoteTests
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
          "[trigger-note.note]",
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
          "[trigger-note.note]",
          "summary = \"first summary\"",
          "body = \"first body\"",
          "urgency = \"critical\"",
          "timeout = \"5\"",
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "[trigger-note.note]",
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
    [ parsesRepeatEvent "off" "false" NoRepeatsToml,
      parsesRepeatEvent "on" "true" AllowRepeatsToml,
      parsesExpected "<none>" txt Nothing (view #repeatEventCfg)
    ]
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "[[trigger-note]]",
          "trigger = \"value\"",
          "[trigger-note.note]",
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
          "repeat-events = " <> flag,
          "[[trigger-note]]",
          "trigger = \"value\"",
          "[trigger-note.note]",
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
          "[trigger-note.note]",
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
          "[trigger-note.note]",
          "summary = \"a summary\""
        ]

parsesExpected ::
  (Eq a, Show a) =>
  String ->
  Text ->
  a ->
  (MultipleToml -> a) ->
  TestTree
parsesExpected = decodeExpected multipleCodec
