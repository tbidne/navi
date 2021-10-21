module Navi.Services.Custom.Multiple.Toml
  ( MultipleToml (..),
    TriggerNoteToml (..),
    multipleCodec,
  )
where

import Navi.Data.NaviNote (NaviNote)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Event (Command (..))
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EventToml
import Navi.Prelude
import Toml (TomlCodec, (.=))
import Toml qualified

multipleCodec :: TomlCodec MultipleToml
multipleCodec =
  MkMultipleToml
    <$> EventToml.commandCodec .= command
    <*> triggerNotesCodec .= triggerNotes
    <*> Toml.dioptional EventToml.repeatEvtCodec .= repeatEvtCfg
    <*> Toml.dioptional EventToml.errorNoteCodec .= errEvtCfg

data MultipleToml = MkMultipleToml
  { command :: Command,
    triggerNotes :: [TriggerNoteToml],
    repeatEvtCfg :: Maybe RepeatEvtToml,
    errEvtCfg :: Maybe ErrorNoteToml
  }
  deriving (Generic, Show)

data TriggerNoteToml = MkTriggerNoteToml
  { trigger :: Text,
    note :: NaviNote
  }
  deriving (Generic, Show)

triggerNotesCodec :: TomlCodec [TriggerNoteToml]
triggerNotesCodec = Toml.list triggerNoteCodec "trigger-note"

triggerNoteCodec :: TomlCodec TriggerNoteToml
triggerNoteCodec =
  MkTriggerNoteToml
    <$> triggerCodec .= trigger
    <*> Toml.table NaviNote.naviNoteCodec "note" .= note
  where
    triggerCodec = Toml.text "trigger"
