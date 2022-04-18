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

    -- * Text
    readFileUtf8Lenient,
    decodeUtf8Lenient,

    -- * Misc utilities
    (>.>),
    (<<$>>),
    maybeToEither,
    monoBimap,
    w16ToInt,

    -- * 'Text' replacements for 'P.String' functions.
    error,
    showt,
    print,

    -- * Base exports
    module X,
  )
where

import Control.Applicative as X (Alternative (..), Applicative (..), (<**>))
import Control.Exception.Safe as X
  ( Exception (..),
    MonadCatch,
    MonadThrow,
    SomeException (..),
    bracket,
    catch,
    handle,
    throw,
    throwM,
    try,
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
import Control.Monad.IO.Class as X (MonadIO (..))
import Control.Monad.Reader as X (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans as X (MonadTrans (..))
import Data.Bifunctor as X (Bifunctor (..))
import Data.Bool as X (Bool (..), not, otherwise, (&&), (||))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Either as X (Either (..), either)
import Data.Eq as X (Eq (..))
import Data.Foldable as X (Foldable (..), length, traverse_)
import Data.Function as X (const, flip, ($), (.))
import Data.Functor as X (Functor (..), ($>), (<$>), (<&>))
import Data.IORef as X (IORef)
import Data.Int as X (Int32)
import Data.Kind as X (Constraint, Type)
import Data.List as X (filter, replicate)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Maybe as X (Maybe (..), fromMaybe, maybe, maybeToList)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ord (..))
import Data.Proxy as X (Proxy (..))
import Data.Semigroup as X (Semigroup (..))
import Data.String as X (String)
import Data.Text as X (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TextEnc
import Data.Text.Encoding.Error qualified as TextEncErr
import Data.Text.IO as X (putStr, putStrLn)
import Data.Traversable as X (Traversable (..))
import Data.Tuple as X (fst, snd)
import Data.Void as X (Void, absurd)
import Data.Word as X (Word16)
import GHC.Enum as X (Bounded (..))
import GHC.Err as X (undefined)
import GHC.Generics as X (Generic)
import GHC.Int as X (Int)
import GHC.Natural as X (Natural (..))
import GHC.Num as X (Num (..))
import GHC.Real as X (Integral (..), fromIntegral)
import GHC.Show as X (Show (..))
import Optics.Core as X (over, set, view, (%), (.~), (^.))
import Optics.TH as X (makeFieldLabelsNoPrefix, makePrismLabels)
import System.IO as X (FilePath, IO)
import Prelude as X (Integer, seq)
import Prelude qualified as P

-- | 'Text' version of 'P.show'.
showt :: P.Show a => a -> Text
showt = T.pack . P.show

-- | 'Text' version of 'error'.
error :: Text -> a
error = P.error . T.unpack

-- | 'Text' version of 'P.print'.
print :: P.Show a => a -> IO ()
print = putStrLn . showt

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

-- | Composed 'fmap'.
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
(<<$>>) = fmap . fmap

infixl 4 <<$>>

-- | Strictly reads a file and leniently converts the contents to UTF8.
--
-- @since 0.1
readFileUtf8Lenient :: MonadIO m => FilePath -> m Text
readFileUtf8Lenient = fmap decodeUtf8Lenient . liftIO . BS.readFile

-- | Lenient UTF8 decode.
--
-- @since 0.1
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TextEnc.decodeUtf8With TextEncErr.lenientDecode

w16ToInt :: Word16 -> Int
w16ToInt = fromIntegral
