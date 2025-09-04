{-# LANGUAGE UndecidableInstances #-}

module Navi.Data.Result
  ( Result (..),
    ResultDefault,

    -- * Elimination
    onResult,
    onErr,
    onOk,
    errorErr,
    failErr,
    throwErr,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (throwM))
import Data.Bifoldable (Bifoldable (bifoldMap))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.String (IsString (fromString))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Optics.Core (An_Iso, LabelOptic, iso)
import Optics.Label (LabelOptic (labelOptic))
import Prelude

-- | General type for error handling, with convenient MonadFail instance.
data Result e a
  = Err e
  | Ok a
  deriving stock (Eq, Functor, Generic, Show)
  deriving anyclass (NFData)

instance
  ( k ~ An_Iso,
    x ~ Either e a,
    y ~ Either e a
  ) =>
  LabelOptic "eitherIso" k (Result e a) (Result e a) x y
  where
  labelOptic =
    iso
      (\case Ok x -> Right x; Err x -> Left x)
      (\case Right x -> Ok x; Left x -> Err x)

type ResultDefault = Result String

instance Applicative (Result e) where
  pure = Ok

  Err x <*> _ = Err x
  _ <*> Err x = Err x
  Ok f <*> Ok x = Ok (f x)

instance Monad (Result e) where
  Err x >>= _ = Err x
  Ok x >>= f = f x

instance Foldable (Result e) where
  foldr f e = onResult (const e) (`f` e)

instance Traversable (Result e) where
  sequenceA = onResult (pure . Err) (fmap Ok)

  traverse f = onResult (pure . Err) (fmap Ok . f)

instance (IsString e) => MonadFail (Result e) where
  fail = Err . fromString

instance Bifunctor Result where
  bimap f g = onResult (Err . f) (Ok . g)

instance Bifoldable Result where
  bifoldMap = onResult

instance Bitraversable Result where
  bitraverse f g = onResult (fmap Err . f) (fmap Ok . g)

-- | Eliminates 'Err' via 'error'.
errorErr :: (HasCallStack) => ResultDefault a -> a
errorErr = onErr error

-- | Eliminates 'Err' via 'MonadFail'.
failErr :: (MonadFail m) => ResultDefault a -> m a
failErr = onResult fail pure

-- | Eliminates 'Err' via 'MonadThrow'.
throwErr ::
  ( Exception e,
    HasCallStack,
    MonadThrow m
  ) =>
  Result e a -> m a
throwErr = onResult throwM pure

-- | General eliminator.
onResult :: (e -> b) -> (a -> b) -> Result e a -> b
onResult f _ (Err err) = f err
onResult _ g (Ok x) = g x

-- | 'Err' eliminator.
onErr :: (e -> a) -> Result e a -> a
onErr f = onResult f id

-- | 'Ok' eliminator.
onOk :: (a -> e) -> Result e a -> e
onOk = onResult id
