module Navi.Services.Custom.Single.Toml
  ( SingleToml (..),
    singleCodec,
  )
where

import DBus.Notify (Note (..))
import Navi.Event (Command (..))
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EventToml
import Navi.Note.Toml qualified as NoteToml
import Navi.Prelude
import Toml (TomlCodec, (.=))
import Toml qualified

data SingleToml = MkSingleToml
  { command :: Command,
    triggerVal :: Text,
    note :: Note,
    repeatEvtCfg :: Maybe RepeatEvtToml,
    errEvtCfg :: Maybe ErrorNoteToml
  }
  deriving (Generic, Show)

singleCodec :: TomlCodec SingleToml
singleCodec =
  MkSingleToml
    <$> EventToml.commandCodec .= command
      <*> Toml.text "trigger" .= triggerVal
      <*> Toml.table NoteToml.noteCodec "note" .= note
      <*> Toml.dioptional EventToml.repeatEvtCodec .= repeatEvtCfg
      <*> Toml.dioptional EventToml.errorNoteCodec .= errEvtCfg
