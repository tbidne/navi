{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the custom multiple service.
module Navi.Services.Custom.Multiple.Toml
  ( MultipleToml (..),
    TriggerNoteToml (..),
    multipleCodec,
  )
where

import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Data.PollInterval (PollInterval (..), pollIntervalCodec)
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EventToml
import Navi.Prelude
import Pythia.Data.Command (Command (..))
import Toml (TomlCodec, (.=))
import Toml qualified

-- | TOML for alerts.
data TriggerNoteToml = MkTriggerNoteToml
  { -- | The text that triggers an alert.
    trigger :: Text,
    -- | The notification to send when triggered.
    note :: NaviNote
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''TriggerNoteToml

-- | TOML for the custom multiple service.
data MultipleToml = MkMultipleToml
  { -- | The command to run.
    command :: Command,
    -- | The alert triggers.
    triggerNotes :: NonEmpty TriggerNoteToml,
    -- | The poll interval.
    pollInterval :: Maybe PollInterval,
    -- | Determines how we treat repeat alerts.
    repeatEvtCfg :: Maybe RepeatEvtToml,
    -- | Determines how we handle errors.
    errEvtCfg :: Maybe ErrorNoteToml
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''MultipleToml

-- | Codec for 'MultipleToml'.
multipleCodec :: TomlCodec MultipleToml
multipleCodec =
  MkMultipleToml
    <$> commandCodec .= command
    <*> triggerNotesCodec .= triggerNotes
    <*> Toml.dioptional pollIntervalCodec .= pollInterval
    <*> Toml.dioptional EventToml.repeatEvtCodec .= repeatEvtCfg
    <*> Toml.dioptional EventToml.errorNoteCodec .= errEvtCfg

triggerNotesCodec :: TomlCodec (NonEmpty TriggerNoteToml)
triggerNotesCodec = Toml.nonEmpty triggerNoteCodec "trigger-note"

triggerNoteCodec :: TomlCodec TriggerNoteToml
triggerNoteCodec =
  MkTriggerNoteToml
    <$> triggerCodec .= trigger
    <*> Toml.table NaviNote.naviNoteCodec "note" .= note
  where
    triggerCodec = Toml.text "trigger"

commandCodec :: TomlCodec Command
commandCodec = Toml.textBy (T.pack . show) (Right . MkCommand) "command"
