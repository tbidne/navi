{-# LANGUAGE CPP #-}
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
  { -- | Determines the log location (i.e. file or stdout).
    location :: Maybe LogLoc,
    -- | Determines the log level.
    severity :: Maybe LogLevel,
    -- | Determines whether to warn/delete large log files.
    sizeMode :: Maybe FilesSizeMode
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ Maybe LogLoc, b ~ Maybe LogLoc) =>
  LabelOptic "location" k Logging Logging a b
  where
  labelOptic =
    lensVL
      $ \f (MkLogging a1 a2 a3) ->
        fmap
          (\b -> MkLogging b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe LogLevel, b ~ Maybe LogLevel) =>
  LabelOptic "severity" k Logging Logging a b
  where
  labelOptic =
    lensVL
      $ \f (MkLogging a1 a2 a3) ->
        fmap
          (\b -> MkLogging a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe FilesSizeMode, b ~ Maybe FilesSizeMode) =>
  LabelOptic "sizeMode" k Logging Logging a b
  where
  labelOptic =
    lensVL
      $ \f (MkLogging a1 a2 a3) ->
        fmap
          (\b -> MkLogging a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

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
    (Just DefPath)
    (Just LevelError)
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

instance
  (k ~ A_Lens, a ~ NonEmpty AnyEvent, b ~ NonEmpty AnyEvent) =>
  LabelOptic "events" k Config Config a b
  where
  labelOptic =
    lensVL
      $ \f (MkConfig a1 a2 a3) ->
        fmap
          (\b -> MkConfig b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Logging, b ~ Logging) =>
  LabelOptic "logging" k Config Config a b
  where
  labelOptic =
    lensVL
      $ \f (MkConfig a1 a2 a3) ->
        fmap
          (\b -> MkConfig a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ NoteSystem ConfigPhaseToml, b ~ NoteSystem ConfigPhaseToml) =>
  LabelOptic "noteSystem" k Config Config a b
  where
  labelOptic =
    lensVL
      $ \f (MkConfig a1 a2 a3) ->
        fmap
          (\b -> MkConfig a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

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
  displayException (TomlError err) = unpackText $ renderTOMLError err
