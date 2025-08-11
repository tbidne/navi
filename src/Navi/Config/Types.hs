{-# LANGUAGE CPP #-}
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

import DBus.Client qualified as DBus
import Data.Bytes (Size (M))
import Data.Bytes qualified as Bytes
import Data.List.NonEmpty ()
import Navi.Config.Phase (ConfigPhase (ConfigPhaseEnv, ConfigPhaseToml))
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

type DBusF :: ConfigPhase -> Type
type family DBusF p where
  DBusF ConfigPhaseToml = ()
  DBusF ConfigPhaseEnv = DBus.Client

-- | Configuration for notification systems.
type NoteSystem :: ConfigPhase -> Type
data NoteSystem p
  = -- | For use with osx.
    AppleScript
  | -- | For use with a running notification server that receives messages
    -- via DBus.
    DBus (DBusF p)
  | -- | For use with the notify-send tool.
    NotifySend

deriving stock instance Eq (NoteSystem ConfigPhaseToml)

deriving stock instance Show (NoteSystem ConfigPhaseToml)

-- | Default notification system i.e. DBus for linux, AppleScript for osx.
defaultNoteSystem :: NoteSystem ConfigPhaseToml
#if OSX
defaultNoteSystem = AppleScript
#else
defaultNoteSystem = DBus ()
#endif

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
    noteSystem :: NoteSystem ConfigPhaseToml
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
