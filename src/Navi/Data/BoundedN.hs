-- | Provides the 'BoundedN' type for safe mathematical
-- operations.
module Navi.Data.BoundedN
  ( -- * Type
    BoundedN (MkBoundedN, unBoundedN),

    -- * Creation
    mkBoundedN,
    unsafeBoundedN,

    -- * Parsing
    boundedNCodec,
  )
where

import Control.Category ((>>>))
import Data.Proxy (Proxy (..))
import GHC.TypeNats (KnownNat, Nat, natVal)
import Navi.Prelude
import Toml
  ( AnyValue (..),
    BiMap (..),
    Key,
    TomlBiMap,
    TomlBiMapError (..),
    TomlCodec,
  )
import Toml qualified

-- | Newtype wrapper over 'Natural'.
type BoundedN :: Nat -> Nat -> Type
newtype BoundedN l u = MkUnsafeBoundedN
  { -- | Unwraps the 'BoundedN'
    unBoundedN :: Natural
  }
  deriving (Eq, Generic, Ord, Show)

-- | Allows pattern matching on 'BoundedN'.
pattern MkBoundedN :: Natural -> BoundedN l u
pattern MkBoundedN n <- MkUnsafeBoundedN n

{-# COMPLETE MkBoundedN #-}

mkBoundedN :: forall l u. (KnownNat l, KnownNat u) => Natural -> Maybe (BoundedN l u)
mkBoundedN n
  | n >= lower && n <= upper = Just $ MkUnsafeBoundedN n
  | otherwise = Nothing
  where
    lower = natVal $ Proxy @l
    upper = natVal $ Proxy @u

unsafeBoundedN :: forall l u. (KnownNat l, KnownNat u) => Natural -> BoundedN l u
unsafeBoundedN n
  | n >= lower && n <= upper = MkUnsafeBoundedN n
  | otherwise =
    error $
      "Passed invalid "
        <> showt n
        <> " bounded by ["
        <> showt lower
        <> ","
        <> showt upper
        <> "]"
  where
    lower = natVal $ Proxy @l
    upper = natVal $ Proxy @u

-- | Parses a TOML 'BoundedN'.
boundedNCodec :: (KnownNat l, KnownNat u) => Key -> TomlCodec (BoundedN l u)
boundedNCodec = Toml.match _BoundedN

_BoundedN :: (KnownNat l, KnownNat u) => TomlBiMap (BoundedN l u) AnyValue
_BoundedN = _BoundedNNatural >>> Toml._Natural

_BoundedNNatural :: (KnownNat l, KnownNat u) => TomlBiMap (BoundedN l u) Natural
_BoundedNNatural = BiMap (Right . unBoundedN) parseBounded
  where
    parseBounded =
      mkBoundedN >.> \case
        Nothing -> Left $ ArbitraryError "Passed integer outside of bounds"
        Just n -> Right n
