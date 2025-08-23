{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Navi.Data.CommandResult
  ( CommandResult (..),
  )
where

import Navi.Data.PollInterval (PollInterval)
import Navi.Prelude

-- The result of running a custom command.
data CommandResult = MkCommandResult
  { -- | Potential output text.
    output :: Maybe Text,
    -- | Potential poll interval.
    pollInterval :: Maybe PollInterval,
    -- | The actual result, corresponding to possible triggers.
    result :: Text
  }
  deriving stock (Show)

makeFieldLabelsNoPrefix ''CommandResult

instance Eq CommandResult where
  x == y = toResult x == toResult y

instance Ord CommandResult where
  x <= y = toResult x <= toResult y

toResult :: CommandResult -> Text
toResult r = r ^. #result
