-- | Provides configuration types.
module Navi.Config.Types
  ( Config (..),
    Logging (..),
    LogLoc (..),
    ConfigErr (..),
  )
where

import Data.List.NonEmpty
import Katip (Severity (..))
import Navi.Data.NonNegative (NonNegative)
import Navi.Event (AnyEvent (..))
import Navi.Prelude
import Optics.Generic (GField (..))
import Optics.Operators ((^.))
import Toml (TomlDecodeError)
import UnexceptionalIO (SomeNonPseudoException)

-- | 'Config' holds the data from 'Navi.Config.Toml.ConfigToml' once it has been processed
-- (e.g., all user defined Events are parsed).
data Config ref = MkConfig
  { -- | Determines how often we query for alerts, in seconds.
    pollInterval :: NonNegative,
    -- | The notification events.
    events :: NonEmpty (AnyEvent ref),
    -- | Logging configuration.
    logging :: Logging
  }
  deriving (Generic)

instance Show (Config ref) where
  show config =
    "MkConfig {pollInterval = "
      <> show (config ^. gfield @"pollInterval")
      <> ", events = "
      <> show (config ^. gfield @"events")
      <> ", logging = "
      <> show (config ^. gfield @"logging")
      <> "}"

-- | Logging configuration.
data Logging = MkLogging
  { -- | Determines the log level.
    severity :: Maybe Severity,
    -- | Deterines the log location (i.e. file or stdout).
    location :: Maybe LogLoc
  }
  deriving (Generic, Show)

-- | Log location configuration.
data LogLoc
  = Stdout
  | File FilePath
  deriving (Generic, Show)

-- | 'ConfigErr' represents the errors we can encounter when attempting to
-- parse a config file.
data ConfigErr
  = FileErr SomeNonPseudoException
  | TomlError [TomlDecodeError]
  | NoEvents
  deriving (Generic, Show)
