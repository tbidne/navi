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
    unless,
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
    logOther,
    logWarn,
  )
import Control.Monad.Reader as X (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans as X (MonadTrans (..))
import Data.Bifunctor as X (Bifunctor (..))
import Data.Bool as X (Bool (..), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.Bytes as X (Bytes (MkBytes), Size (B))
import Data.Char as X (Char)
import Data.Either as X (Either (..), either)
import Data.Eq as X (Eq (..))
import Data.Foldable as X (Foldable (..), for_, length, traverse_)
import Data.Function as X (const, flip, id, ($), (.))
import Data.Functor as X (Functor (..), ($>), (<$>), (<&>))
import Data.Int as X (Int32)
import Data.Kind as X (Constraint, Type)
import Data.List as X (all, filter, replicate, zipWith)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Maybe as X (Maybe (..), fromMaybe, maybe, maybeToList)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ord (..))
import Data.Proxy as X (Proxy (..))
import Data.Semigroup as X (Semigroup (..))
import Data.Sequence as X (Seq ((:<|), (:|>)))
import Data.String as X (IsString (fromString), String)
import Data.Text as X (Text, concat, pack, unpack)
import Data.Traversable as X (Traversable (..))
import Data.Tuple as X (fst, snd, uncurry)
import Data.Void as X (Void, absurd)
import Data.Word as X (Word16, Word8)
import Effects.Concurrent.Async as X (MonadAsync)
import Effects.Concurrent.STM as X
  ( MonadSTM,
    TBQueue,
    newTBQueueM,
    readTBQueueM,
    tryReadTBQueueM,
    writeTBQueueM,
  )
import Effects.Concurrent.Thread as X (MonadThread)
import Effects.Exception as X
  ( Exception (..),
    MonadCatch,
    MonadMask,
    MonadThrow,
    SomeException,
    addCS,
    bracket,
    catchAny,
    catchCS,
    finally,
    mask,
    throwCS,
    throwM,
  )
import Effects.FileSystem.FileReader as X
  ( MonadFileReader,
    readFileUtf8ThrowM,
  )
import Effects.FileSystem.FileWriter as X (MonadFileWriter, writeFileUtf8)
import Effects.FileSystem.HandleWriter as X
  ( Handle,
    IOMode (..),
    MonadHandleWriter (hClose, hFlush, hPut, openBinaryFile),
  )
import Effects.FileSystem.Path as X (Path, (</>))
import Effects.FileSystem.PathReader as X (MonadPathReader)
import Effects.IORef as X
  ( IORef,
    MonadIORef (modifyIORef', newIORef, readIORef, writeIORef),
  )
import Effects.System.Terminal as X (MonadTerminal, putStrLn, putTextLn)
import GHC.Enum as X (Bounded (..))
import GHC.Err as X (undefined)
import GHC.Float as X (Double)
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
import System.IO as X (IO)
import TOML as X
  ( DecodeTOML (..),
    TOMLError (..),
    Value (Integer, String),
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
import Prelude as X (Integer, seq)
import Prelude qualified as P

-- | 'Text' version of 'P.show'.
showt :: (P.Show a) => a -> Text
showt = pack . P.show

-- | 'Text' version of 'error'.
error :: Text -> a
error = P.error . unpack

-- | Safe @head@.
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : _) = Just x

-- | Transforms 'Maybe' to 'Either'.
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just x) = Right x

-- | Convenience function for mapping @(a -> b)@ over a monomorphic bifunctor.
monoBimap :: (Bifunctor p) => (a -> b) -> p a a -> p b b
monoBimap f = bimap f f

-- | Flipped version of '(.)'.
(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)

infixr 8 >.>

-- | Composed 'fmap'.
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
(<<$>>) = fmap . fmap

infixl 4 <<$>>
