{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides configuration types.
module Navi.Config.Types
  ( -- * Config
    Config (..),
    ConfigErr (..),

    -- * Logging
    Logging (..),
    LogLoc (..),
    defaultLogging,

    -- * Note System
    NoteSystem (..),
    defaultNoteSystem,
  )
where

import Data.List.NonEmpty
import Data.Text qualified as T
import Katip (Severity (..))
import Navi.Event (AnyEvent (..))
import Navi.Prelude
import Toml (TomlDecodeError)
import Toml qualified

-- | Log location configuration.
data LogLoc
  = DefPath
  | Stdout
  | File !FilePath
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''LogLoc

-- | Logging configuration.
data Logging = MkLogging
  { -- | Determines the log level.
    severity :: !Severity,
    -- | Deterines the log location (i.e. file or stdout).
    location :: !LogLoc
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Logging

-- | Configuration for notification systems.
data NoteSystem
  = -- | For use with a running notification server that receives messages
    -- via DBus.
    DBus
  | -- | For use with the notify-send tool.
    NotifySend
  deriving stock (Eq, Show)

makePrismLabels ''NoteSystem

-- | Default notification system i.e. DBus.
defaultNoteSystem :: NoteSystem
defaultNoteSystem = DBus

-- | Default logging i.e. log errors and use the default path.
defaultLogging :: Logging
defaultLogging = MkLogging ErrorS DefPath

-- | 'Config' holds the data from 'Navi.Config.Toml.ConfigToml' once it has been processed
-- (e.g., all user defined Events are parsed).
data Config ref = MkConfig
  { -- | The notification events.
    events :: !(NonEmpty (AnyEvent ref)),
    -- | Logging configuration.
    logging :: !Logging,
    -- | The notification system to use.
    noteSystem :: !NoteSystem
  }

makeFieldLabelsNoPrefix ''Config

instance Show (Config ref) where
  show config =
    "MkConfig {events = "
      <> show (config ^. #events)
      <> ", logging = "
      <> show (config ^. #logging)
      <> "}"

-- | 'ConfigErr' represents the errors we can encounter when attempting to
-- parse a config file.
data ConfigErr
  = FileErr !SomeException
  | TomlError ![TomlDecodeError]
  | NoEvents
  deriving stock (Show)

instance Exception ConfigErr where
  displayException (FileErr ex) = "Error reading file: <" <> displayException ex <> ">"
  displayException NoEvents = "No events found"
  displayException (TomlError errs) = unpack $ Toml.prettyTomlDecodeErrors errs

makeFieldLabelsNoPrefix ''ConfigErr
