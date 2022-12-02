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
    writeFileUtf8,
    decodeUtf8Lenient,

    -- * Exceptions
    catchAny,

    -- * Misc utilities
    (>.>),
    (<<$>>),
    maybeToEither,
    monoBimap,

    -- * 'Text' replacements for 'P.String' functions.
    error,
    showt,

    -- * Base exports
    module X,
  )
where

import Control.Applicative as X (Alternative (..), Applicative (..), (<**>))
import Control.DeepSeq as X (NFData)
import Control.Monad as X
  ( Monad (..),
    forever,
    join,
    void,
    when,
    (<=<),
    (=<<),
    (>=>),
  )
import Control.Monad.Fail as X (MonadFail (..))
import Control.Monad.IO.Class as X (MonadIO (..))
import Control.Monad.Logger as X
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelWarn),
    LogStr,
    MonadLogger (monadLoggerLog),
    logDebug,
    logError,
    logInfo,
    logWarn,
  )
import Control.Monad.Reader as X (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans as X (MonadTrans (..))
import Data.Bifunctor as X (Bifunctor (..))
import Data.Bool as X (Bool (..), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString, hPut)
import Data.ByteString qualified as BS
import Data.Char as X (Char)
import Data.Either as X (Either (..), either)
import Data.Eq as X (Eq (..))
import Data.Foldable as X (Foldable (..), for_, length, traverse_)
import Data.Function as X (const, flip, id, ($), (.))
import Data.Functor as X (Functor (..), ($>), (<$>), (<&>))
import Data.IORef as X (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int as X (Int32)
import Data.Kind as X (Constraint, Type)
import Data.List as X (all, filter, replicate, zipWith)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Maybe as X (Maybe (..), fromMaybe, maybe, maybeToList)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ord (..))
import Data.Proxy as X (Proxy (..))
import Data.Semigroup as X (Semigroup (..))
import Data.Sequence as X (Seq, (<|), (|>))
import Data.String as X (IsString (fromString), String)
import Data.Text as X (Text, concat, pack, unpack)
import Data.Text.Encoding qualified as TextEnc
import Data.Text.Encoding.Error qualified as TextEncErr
import Data.Traversable as X (Traversable (..))
import Data.Tuple as X (fst, snd, uncurry)
import Data.Void as X (Void, absurd)
import Data.Word as X (Word16, Word8)
import Effects.MonadCallStack as X
  ( MonadCallStack (throwWithCallStack),
    catch,
    prettyAnnotated,
    try,
  )
import Effects.MonadFsReader as X (MonadFsReader)
import Effects.MonadThread as X (MonadThread)
import GHC.Enum as X (Bounded (..))
import GHC.Err as X (undefined)
import GHC.Generics as X (Generic)
import GHC.Int as X (Int)
import GHC.Natural as X (Natural (..))
import GHC.Num as X (Num (..))
import GHC.Real as X (Integral (..), fromIntegral)
import GHC.Show as X (Show (..))
import GHC.Stack as X (HasCallStack)
import Optics.Core as X
  ( AffineTraversal',
    Iso',
    Lens',
    Traversal',
    lens,
    over',
    preview,
    review,
    set',
    view,
    (%),
    (%?),
    (.~),
    (^.),
    (^?),
    _1,
    _2,
    _Just,
  )
import Optics.TH as X (makeFieldLabelsNoPrefix, makePrisms)
import System.IO as X
  ( FilePath,
    Handle,
    IO,
    IOMode (AppendMode),
    hFlush,
    openFile,
    putStrLn,
  )
import TOML as X
  ( DecodeTOML (..),
    TOMLError (..),
    Value (..),
    decode,
    getArrayOf,
    getField,
    getFieldOpt,
    getFieldOptWith,
    getFieldWith,
    invalidValue,
    makeDecoder,
    renderTOMLError,
    typeMismatch,
  )
import TOML.Decode as X (Decoder)
import UnliftIO as X (MonadUnliftIO)
import UnliftIO.Exception as X
  ( Exception (..),
    SomeException,
    bracket,
    finally,
    handle,
    throwIO,
    tryAny,
  )
import UnliftIO.STM as X (TBQueue)
import Prelude as X (Integer, seq)
import Prelude qualified as P

-- | 'Text' version of 'P.show'.
showt :: P.Show a => a -> Text
showt = pack . P.show
{-# INLINEABLE showt #-}

-- | 'Text' version of 'error'.
error :: Text -> a
error = P.error . unpack
{-# INLINEABLE error #-}

-- | Safe @head@.
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x
{-# INLINEABLE headMaybe #-}

-- | Transforms 'Maybe' to 'Either'.
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just x) = Right x
{-# INLINEABLE maybeToEither #-}

-- | Convenience function for mapping @(a -> b)@ over a monomorphic bifunctor.
monoBimap :: Bifunctor p => (a -> b) -> p a a -> p b b
monoBimap f = bimap f f
{-# INLINEABLE monoBimap #-}

-- | Flipped version of '(.)'.
(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)
{-# INLINEABLE (>.>) #-}

infixr 8 >.>

-- | Composed 'fmap'.
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
(<<$>>) = fmap . fmap
{-# INLINEABLE (<<$>>) #-}

infixl 4 <<$>>

-- | Writes the text to the file.
--
-- @since 0.1
writeFileUtf8 :: FilePath -> Text -> IO ()
writeFileUtf8 fp = BS.writeFile fp . TextEnc.encodeUtf8
{-# INLINEABLE writeFileUtf8 #-}

-- | Strictly reads a file and leniently converts the contents to UTF8.
--
-- @since 0.1
readFileUtf8Lenient :: MonadIO m => FilePath -> m Text
readFileUtf8Lenient = fmap decodeUtf8Lenient . liftIO . BS.readFile
{-# INLINEABLE readFileUtf8Lenient #-}

-- | Lenient UTF8 decode.
--
-- @since 0.1
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = TextEnc.decodeUtf8With TextEncErr.lenientDecode
{-# INLINEABLE decodeUtf8Lenient #-}

-- | @since 0.1
catchAny ::
  ( HasCallStack,
    MonadUnliftIO m
  ) =>
  m a ->
  (SomeException -> m a) ->
  m a
catchAny = catch @SomeException
