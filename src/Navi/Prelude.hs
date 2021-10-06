-- | Custom prelude. The idea is to:
--
-- * Re-export useful prelude functions/types
-- * Export various functions/types from base
-- * Export new functions meant to address prelude limitations
--   (e.g. total replacements for partial functions).
--
-- This is not a comprehensive replacement for Prelude, just the
-- functionality needed for this application. Thus it is natural to
-- add new functionality/exports here over time.
module Navi.Prelude
  ( -- * Total versions of partial functions
    headMaybe,

    -- * Misc utilities
    (>.>),
    maybeToEither,
    monoBimap,

    -- * 'Text' replacements for 'P.String' functions.
    error,
    showt,

    -- * Prelude exports
    module X,
  )
where

import Control.Applicative as X
  ( Alternative (..),
    Applicative (..),
    liftA2,
    (<**>),
  )
import Control.Monad as X
  ( Monad (..),
    forever,
    join,
    void,
    (<=<),
    (=<<),
    (>=>),
  )
import Data.Bifunctor as X (Bifunctor (..))
import Data.Either as X (Either (..), either)
import Data.Foldable as X
  ( Foldable
      ( fold,
        foldMap,
        foldMap',
        foldl',
        foldr
      ),
    length,
    traverse_,
  )
import Data.Functor as X
  ( Functor (..),
    ($>),
    (<$>),
    (<&>),
  )
import Data.Kind as X (Constraint, Type)
import Data.List as X (filter)
import Data.Maybe as X (Maybe (..), maybe)
import Data.Monoid as X (Monoid (..))
import Data.Semigroup as X (Semigroup (..))
import Data.Text as X (Text)
import Data.Text qualified as T
import Data.Text.IO as X (putStr, putStrLn)
import Data.Traversable as X (Traversable (..))
import GHC.Natural as X (Natural (..))
import Prelude as X
  ( Bool (..),
    Bounded (..),
    Eq (..),
    FilePath,
    IO,
    Int,
    Integer,
    Integral (..),
    Num (..),
    Ord (..),
    Show (..),
    String,
    const,
    flip,
    fromIntegral,
    fst,
    not,
    otherwise,
    replicate,
    seq,
    snd,
    ($),
    (&&),
    (++),
    (.),
    (||),
  )
import Prelude qualified as P

-- | 'Text' version of 'P.show'.
showt :: P.Show a => a -> Text
showt = T.pack . P.show

-- | 'Text' version of 'error'.
error :: Text -> a
error = P.error . T.unpack

-- | Safe @head@.
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- | Transforms 'Maybe' to 'Either'.
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just x) = Right x

-- | Convenience function for mapping @(a -> b)@ over a monomorphic bifunctor.
monoBimap :: Bifunctor p => (a -> b) -> p a a -> p b b
monoBimap f = bimap f f

-- | Flipped version of '(.)'.
(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixr 8 >.>