{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the custom multiple service.
module Navi.Services.Custom.Multiple.Toml
  ( MultipleToml (..),
    TriggerNoteToml (..),
  )
where

import Navi.Data.NaviNote (NaviNote)
import Navi.Data.PollInterval (PollInterval, pollIntervalOptDecoder)
import Navi.Event.Toml
  ( ErrorNoteToml,
    RepeatEventToml,
    errorNoteOptDecoder,
    repeatEventOptDecoder,
  )
import Navi.Prelude
import Navi.Utils (commandDecoder)
import Pythia.Data.Command (Command)

-- | TOML for alerts.
data TriggerNoteToml = MkTriggerNoteToml
  { -- | The text that triggers an alert.
    trigger :: Text,
    -- | The notification to send when triggered.
    note :: NaviNote
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''TriggerNoteToml

-- | @since 0.1
instance DecodeTOML TriggerNoteToml where
  tomlDecoder =
    MkTriggerNoteToml
      <$> getField "trigger"
      <*> getField "note"

-- | TOML for the custom multiple service.
data MultipleToml = MkMultipleToml
  { -- | The command to run.
    command :: Command,
    -- | An optional name to be used with logging.
    name :: Maybe Text,
    -- | The alert triggers.
    triggerNotes :: NonEmpty TriggerNoteToml,
    -- | The poll interval.
    pollInterval :: Maybe PollInterval,
    -- | Determines how we treat repeat alerts.
    repeatEventCfg :: Maybe RepeatEventToml,
    -- | Determines how we handle errors.
    errEventCfg :: Maybe ErrorNoteToml
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''MultipleToml

-- | @since 0.1
instance DecodeTOML MultipleToml where
  tomlDecoder =
    MkMultipleToml
      <$> commandDecoder
      <*> getFieldOpt "name"
      <*> getFieldWith tomlDecoder "trigger-note"
      <*> pollIntervalOptDecoder
      <*> repeatEventOptDecoder
      <*> errorNoteOptDecoder
