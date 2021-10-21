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

data Config ref = MkConfig
  { pollInterval :: NonNegative,
    events :: NonEmpty (AnyEvent ref),
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

data Logging = MkLogging
  { severity :: Maybe Severity,
    location :: Maybe LogLoc
  }
  deriving (Generic, Show)

data LogLoc
  = Stdout
  | File FilePath
  deriving (Generic, Show)

data ConfigErr
  = FileErr SomeNonPseudoException
  | TomlError [TomlDecodeError]
  | NoEvents
  deriving (Generic, Show)
