{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the custom single service.
module Navi.Services.Custom.Single.Toml
  ( SingleToml (..),
  )
where

import Navi.Data.CommandResultParser (CommandResultParserToml, commandResultParserDecoder)
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

-- | Codec for 'SingleToml'.
data SingleToml = MkSingleToml
  { -- | The command to run.
    command :: Command,
    -- | An optional name to be used with logging.
    name :: Maybe Text,
    -- | The alert trigger.
    triggerVal :: Text,
    -- | Custom parsing.
    parser :: Maybe CommandResultParserToml,
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

-- | @since 0.1
instance DecodeTOML SingleToml where
  tomlDecoder =
    MkSingleToml
      <$> commandDecoder
      <*> getFieldOpt "name"
      <*> getField "trigger"
      <*> commandResultParserDecoder
      <*> pollIntervalOptDecoder
      <*> getField "note"
      <*> repeatEventOptDecoder
      <*> errorNoteOptDecoder
