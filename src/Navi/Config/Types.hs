{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides configuration types.
module Navi.Config.Types
  ( Config (..),
    Logging (..),
    LogLoc (..),
    ConfigErr (..),
  )
where

import Data.List.NonEmpty
import Data.Text qualified as T
import Katip (Severity (..))
import Navi.Event (AnyEvent (..))
import Navi.Prelude
import Pythia.Class.Printer qualified as Printer
import Toml (TomlDecodeError)

-- | Log location configuration.
data LogLoc
  = Stdout
  | File FilePath
  deriving (Show)

makeFieldLabelsNoPrefix ''LogLoc

-- | Logging configuration.
data Logging = MkLogging
  { -- | Determines the log level.
    severity :: Maybe Severity,
    -- | Deterines the log location (i.e. file or stdout).
    location :: Maybe LogLoc
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''Logging

-- | 'Config' holds the data from 'Navi.Config.Toml.ConfigToml' once it has been processed
-- (e.g., all user defined Events are parsed).
data Config ref = MkConfig
  { -- | Determines how often we query for alerts, in seconds.
    pollInterval :: Word16,
    -- | The notification events.
    events :: NonEmpty (AnyEvent ref),
    -- | Logging configuration.
    logging :: Logging
  }

makeFieldLabelsNoPrefix ''Config

instance Show (Config ref) where
  show config =
    "MkConfig {pollInterval = "
      <> show (config ^. #pollInterval)
      <> ", events = "
      <> show (config ^. #events)
      <> ", logging = "
      <> show (config ^. #logging)
      <> "}"

-- | 'ConfigErr' represents the errors we can encounter when attempting to
-- parse a config file.
data ConfigErr
  = FileErr SomeException
  | TomlError [TomlDecodeError]
  | NoEvents
  deriving stock (Show)

instance Exception ConfigErr where
  displayException (FileErr ex) = "Error reading file: <" <> displayException ex <> ">"
  displayException NoEvents = "No events found"
  displayException (TomlError errs) =
    T.unpack $
      header <> delim
        <> Printer.joinX delim (fmap show errs)
    where
      delim = "\n - "
      header = "Tomls errors:"

makeFieldLabelsNoPrefix ''ConfigErr
