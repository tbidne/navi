-- | Provides the 'BoundedN' type for safe mathematical
-- operations.
module Navi.Data.BoundedN
  ( BoundedN (MkBoundedN, unBounded),
    mkBoundedN,
    unsafeBoundedN,
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Natural (Natural)
import GHC.TypeNats (KnownNat, Nat, natVal)

-- | Newtype wrapper over 'Natural'.
type BoundedN :: Nat -> Nat -> Type
newtype BoundedN l u = MkUnsafeBoundedN
  { -- | Unwraps the 'BoundedN'
    unBounded :: Natural
  }
  deriving (Eq, Ord, Show)

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
        <> show n
        <> " bounded by ["
        <> show lower
        <> ","
        <> show upper
        <> "]"
  where
    lower = natVal $ Proxy @l
    upper = natVal $ Proxy @u