-- | Provides the 'NonNegative' type for safe mathematical
-- operations.
module Navi.Data.NonNegative
  ( -- * Type
    NonNegative (MkNonNegative, unNonNegative),

    -- * Creation
    mkNonNegative,
    readNonNegative,
    unsafeNonNegative,

    -- * Parsing
    nonNegativeCodec,
  )
where

import Control.Category ((>>>))
import Navi.Prelude
import Text.Read qualified as TR
import Toml
  ( AnyValue (..),
    BiMap (..),
    Key,
    TomlBiMap,
    TomlBiMapError (..),
    TomlCodec,
  )
import Toml qualified

-- | Newtype wrapper over 'Int'.
newtype NonNegative = MkUnsafeNonNegative
  { -- | Unwraps the 'NonNegative'
    unNonNegative :: Int
  }
  deriving (Eq, Generic, Ord, Show)

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

readNonNegative :: String -> Maybe NonNegative
readNonNegative = TR.readMaybe >=> mkNonNegative

-- | Parses a TOML 'NonNegative'.
nonNegativeCodec :: Key -> TomlCodec NonNegative
nonNegativeCodec = Toml.match _NonNegative

_NonNegative :: TomlBiMap NonNegative AnyValue
_NonNegative = _NonNegativeInt >>> Toml._Int

_NonNegativeInt :: TomlBiMap NonNegative Int
_NonNegativeInt = BiMap (Right . unNonNegative) parseNN
  where
    parseNN =
      mkNonNegative >.> \case
        Nothing -> Left $ ArbitraryError "Passed negative to mkNonNegative"
        Just n -> Right n
