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
    FilesSizeMode (..),
    defaultLogging,
    defaultSizeMode,

    -- * Note System
    NoteSystem (..),
    defaultNoteSystem,
  )
where

import Data.Bytes (Size (M))
import Data.Bytes qualified as Bytes
import Data.List.NonEmpty ()
import Navi.Event (AnyEvent)
import Navi.Prelude

-- | Log location configuration.
data LogLoc
  = DefPath
  | Stdout
  | File OsPath
  deriving stock (Eq, Show)

-- | Determines what to do if the log file surpasses the given size
-- threshold.
data FilesSizeMode
  = -- | Print a warning.
    FilesSizeModeWarn (Bytes B Natural)
  | -- | Delete the file.
    FilesSizeModeDelete (Bytes B Natural)
  deriving stock (Eq, Show)

-- | Logging configuration.
data Logging = MkLogging
  { -- | Determines the log level.
    severity :: Maybe LogLevel,
    -- | Determines the log location (i.e. file or stdout).
    location :: Maybe LogLoc,
    -- | Determines whether to warn/delete large log files.
    sizeMode :: Maybe FilesSizeMode
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

-- | Default notification system i.e. DBus.
defaultNoteSystem :: NoteSystem
defaultNoteSystem = DBus

-- | Default logging i.e. log errors and use the default path.
defaultLogging :: Logging
defaultLogging =
  MkLogging
    (Just LevelError)
    (Just DefPath)
    (Just defaultSizeMode)

-- | @since 0.1
defaultSizeMode :: FilesSizeMode
defaultSizeMode = FilesSizeModeDelete $ Bytes.convert_ fiftyMb
  where
    fiftyMb = MkBytes @M 50

-- | 'Config' holds the data from 'Navi.Config.Toml.ConfigToml' once it has been processed
-- (e.g., all user defined Events are parsed).
data Config = MkConfig
  { -- | The notification events.
    events :: NonEmpty AnyEvent,
    -- | Logging configuration.
    logging :: Logging,
    -- | The notification system to use.
    noteSystem :: NoteSystem
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''Config

-- | 'ConfigErr' represents the errors we can encounter when attempting to
-- parse a config file.
data ConfigErr
  = FileErr SomeException
  | TomlError TOMLError
  | NoEvents
  deriving stock (Show)

instance Exception ConfigErr where
  displayException (FileErr ex) = "Error reading file: <" <> displayException ex <> ">"
  displayException NoEvents = "No events found"
  displayException (TomlError err) = unpack $ renderTOMLError err
