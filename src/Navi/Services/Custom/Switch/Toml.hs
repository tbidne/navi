{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the custom switch service.
module Navi.Services.Custom.Switch.Toml
  ( SwitchToml (..),
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

-- | Codec for 'SwitchToml'.
data SwitchToml = MkSwitchToml
  { -- | The command to run.
    command :: Command,
    -- | An optional name to be used with logging.
    name :: Maybe Text,
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

makeFieldLabelsNoPrefix ''SwitchToml

-- | @since 0.1
instance DecodeTOML SwitchToml where
  tomlDecoder =
    MkSwitchToml
      <$> commandDecoder
      <*> getFieldOpt "name"
      <*> getField "trigger"
      <*> pollIntervalOptDecoder
      <*> getField "note"
      <*> repeatEventOptDecoder
      <*> errorNoteOptDecoder
