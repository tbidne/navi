module Navi.Services.Types
  ( ServiceResult (..),
    ServiceErr (..),
  )
where

import DBus.Notify (Note)
import Data.Text (Text)

data ServiceResult
  = Err ServiceErr
  | None
  | Alert Note
  deriving (Show)

data ServiceErr = MkServiceErr
  { serviceName :: Text,
    shortMsg :: Text,
    longMsg :: Text
  }
  deriving (Show)