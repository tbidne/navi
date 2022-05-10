{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the custom single service.
module Navi.Services.Custom.Single.Toml
  ( SingleToml (..),
    singleCodec,
  )
where

import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Data.PollInterval (PollInterval (..), pollIntervalCodec)
import Navi.Event.Toml (ErrorNoteToml, RepeatEventToml)
import Navi.Event.Toml qualified as EventToml
import Navi.Prelude
import Pythia.Data.Command (Command (..))
import Toml (TomlCodec, (.=))
import Toml qualified

-- | Codec for 'SingleToml'.
data SingleToml = MkSingleToml
  { -- | The command to run.
    command :: Command,
    -- | The alert trigger.
    triggerVal :: Text,
    -- | The poll interval.
    pollInterval :: Maybe PollInterval,
    -- | The notification to send.
    note :: NaviNote,
    -- | Determines how we treat repeat alerts.
    repeatEventCfg :: Maybe RepeatEventToml,
    -- | Determines how we handle errors.
    errEventCfg :: Maybe ErrorNoteToml
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''SingleToml

-- | Codec for 'SingleToml'.
singleCodec :: TomlCodec SingleToml
singleCodec =
  MkSingleToml
    <$> commandCodec .= command
      <*> Toml.text "trigger" .= triggerVal
      <*> Toml.dioptional pollIntervalCodec .= pollInterval
      <*> Toml.table NaviNote.naviNoteCodec "note" .= note
      <*> Toml.dioptional EventToml.repeatEventCodec .= repeatEventCfg
      <*> Toml.dioptional EventToml.errorNoteCodec .= errEventCfg

commandCodec :: TomlCodec Command
commandCodec = Toml.textBy (T.pack . show) (Right . MkCommand) "command"
