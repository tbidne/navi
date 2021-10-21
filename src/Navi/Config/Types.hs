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
import Toml (TomlDecodeError)
import UnexceptionalIO (SomeNonPseudoException)

data Config ref = MkConfig
  { pollInterval :: NonNegative,
    events :: NonEmpty (AnyEvent ref),
    logging :: Logging
  }

data Logging = MkLogging
  { severity :: Maybe Severity,
    location :: Maybe LogLoc
  }
  deriving (Show)

data LogLoc
  = Stdout
  | File FilePath
  deriving (Show)

data ConfigErr
  = FileErr SomeNonPseudoException
  | TomlError [TomlDecodeError]
  | NoEvents
  deriving (Show)