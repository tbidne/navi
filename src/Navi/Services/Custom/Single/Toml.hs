module Navi.Services.Custom.Single.Toml
  ( SingleToml (..),
    singleCodec,
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

data SingleToml = MkSingleToml
  { command :: Command,
    triggerVal :: Text,
    note :: NaviNote,
    repeatEvtCfg :: Maybe RepeatEvtToml,
    errEvtCfg :: Maybe ErrorNoteToml
  }
  deriving (Generic, Show)

singleCodec :: TomlCodec SingleToml
singleCodec =
  MkSingleToml
    <$> EventToml.commandCodec .= command
      <*> Toml.text "trigger" .= triggerVal
      <*> Toml.table NaviNote.naviNoteCodec "note" .= note
      <*> Toml.dioptional EventToml.repeatEvtCodec .= repeatEvtCfg
      <*> Toml.dioptional EventToml.errorNoteCodec .= errEvtCfg
