-- | Provides the 'NonNegative' type for safe mathematical
-- operations.
module Navi.Data.NonNegative
  ( NonNegative (MkNonNegative, unNonNegative),
    mkNonNegative,
    unsafeNonNegative,
  )
where

import Navi.Prelude

-- | Newtype wrapper over 'Int'.
newtype NonNegative = MkUnsafeNonNegative
  { -- | Unwraps the 'NonNegative'
    unNonNegative :: Int
  }
  deriving (Eq, Ord, Show)

-- | Allows pattern matching on 'NonNegative'.
pattern MkNonNegative :: Int -> NonNegative
pattern MkNonNegative n <- MkUnsafeNonNegative n

{-# COMPLETE MkNonNegative #-}

-- | Smart constructor for 'NonNegative'.
--
-- Examples:
--
-- >>> mkNonNegative 7
-- Just (MkNonNegative {getNonNegative = 7})
--
-- >>> mkNonNegative (-2)
-- Nothing
mkNonNegative :: Int -> Maybe NonNegative
mkNonNegative n
  | n >= 0 = Just $ MkUnsafeNonNegative n
  | otherwise = Nothing

-- | Unsafe constructor for 'NonNegative', intended to be used with
-- known constants, e.g., @unsafeNonNegative 7@. Exercise restraint!
unsafeNonNegative :: Int -> NonNegative
unsafeNonNegative n
  | n >= 0 = MkUnsafeNonNegative n
  | otherwise =
    error $
      "Passed negative "
        <> showt n
        <> " to unsafeNonNegative!"
