module Navi.Services.Custom.Types
  ( Trigger (..),
  )
where

import Navi.Prelude

newtype Trigger = MkTrigger Text
  deriving (Eq, Show)
