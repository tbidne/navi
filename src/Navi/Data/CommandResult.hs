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

instance
  (k ~ A_Lens, a ~ Maybe Text, b ~ Maybe Text) =>
  LabelOptic "output" k CommandResult CommandResult a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandResult a1 a2 a3) ->
        fmap
          (\b -> MkCommandResult b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe PollInterval, b ~ Maybe PollInterval) =>
  LabelOptic "pollInterval" k CommandResult CommandResult a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandResult a1 a2 a3) ->
        fmap
          (\b -> MkCommandResult a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "result" k CommandResult CommandResult a b
  where
  labelOptic =
    lensVL
      $ \f (MkCommandResult a1 a2 a3) ->
        fmap
          (\b -> MkCommandResult a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

instance Eq CommandResult where
  x == y = toResult x == toResult y

instance Ord CommandResult where
  x <= y = toResult x <= toResult y

toResult :: CommandResult -> Text
toResult r = r ^. #result
