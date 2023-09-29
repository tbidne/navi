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
    showt,

    -- * Base exports
    module X,
  )
where

import Control.Applicative as X
  ( Alternative ((<|>)),
    Applicative (liftA2, pure, (*>), (<*>)),
    (<**>),
  )
import Control.DeepSeq as X (NFData)
import Control.Monad as X
  ( Monad ((>>=)),
    forever,
    join,
    unless,
    void,
    when,
    (<=<),
    (=<<),
    (>=>),
  )
import Control.Monad.Fail as X (MonadFail (fail))
import Control.Monad.IO.Class as X (MonadIO (liftIO))
import Data.Bifunctor as X (Bifunctor (bimap, first, second))
import Data.Bool as X (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString as X (ByteString)
import Data.Bytes as X (Bytes (MkBytes), Size (B))
import Data.Char as X (Char)
import Data.Either as X (Either (Left, Right), either)
import Data.Eq as X (Eq ((/=), (==)))
import Data.Foldable as X (Foldable (foldl', foldr), elem, for_, length, traverse_)
import Data.Function as X (const, flip, id, ($), (.))
import Data.Functor as X (Functor (fmap), ($>), (<$>), (<&>))
import Data.Int as X (Int32)
import Data.Kind as X (Constraint, Type)
import Data.List as X (all, filter, replicate, zipWith)
import Data.List.NonEmpty as X (NonEmpty ((:|)))
import Data.Maybe as X (Maybe (Just, Nothing), fromMaybe, maybe, maybeToList)
import Data.Monoid as X (Monoid (mempty), mconcat)
import Data.Ord as X (Ord ((<=), (>)))
import Data.Proxy as X (Proxy (Proxy))
import Data.Semigroup as X (Semigroup ((<>)))
import Data.Sequence as X (Seq ((:<|), (:|>)))
import Data.String as X (IsString (fromString), String)
import Data.Text as X (Text, concat, pack, unpack)
import Data.Traversable as X (Traversable (traverse))
import Data.Tuple as X (fst, snd, uncurry)
import Data.Void as X (Void, absurd)
import Data.Word as X (Word16, Word8)
import Effectful as X
  ( Dispatch (Dynamic, Static),
    DispatchOf,
    Eff,
    Effect,
    IOE,
    runEff,
    type (:>),
  )
import Effectful.Concurrent as X (Concurrent)
import Effectful.Concurrent.STM.Static as X
  ( TBQueue,
    flushTBQueueA,
    newTBQueueA,
    readTBQueueA,
    tryReadTBQueueA,
    writeTBQueueA,
  )
import Effectful.Dispatch.Dynamic as X (interpret)
import Effectful.Exception as X
  ( Exception (displayException),
    MonadCatch,
    MonadMask,
    MonadThrow,
    SomeException,
    bracket,
    catch,
    catchAny,
    finally,
    mask,
    throwM,
  )
import Effectful.FileSystem.FileReader.Dynamic as X
  ( FileReaderDynamic,
    readFileUtf8ThrowM,
  )
import Effectful.FileSystem.FileWriter.Dynamic as X
  ( FileWriterDynamic,
    writeFileUtf8,
  )
import Effectful.FileSystem.HandleWriter.Dynamic as X
  ( Handle,
    HandleWriterDynamic,
    IOMode (AppendMode, WriteMode),
    hClose,
    hFlush,
    hPut,
    openBinaryFile,
  )
import Effectful.FileSystem.PathReader.Dynamic as X (PathReaderDynamic)
import Effectful.FileSystem.PathWriter.Dynamic as X (PathWriterDynamic)
import Effectful.FileSystem.Utils as X (OsPath, (</>))
import Effectful.IORef.Static as X
  ( IORef,
    IORefStatic,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Effectful.Logger.Dynamic as X
  ( LogLevel (LevelDebug, LevelError, LevelInfo, LevelWarn),
    LogStr,
    LoggerDynamic (LoggerLog),
    logDebug,
    logError,
    logInfo,
    logOther,
    logWarn,
  )
import Effectful.LoggerNS.Dynamic as X (LoggerNSDynamic)
import Effectful.Process.Typed as X (TypedProcess)
import Effectful.Reader.Static as X (Reader, asks)
import Effectful.Terminal.Dynamic as X (TerminalDynamic, putStrLn, putTextLn)
import Effectful.Time.Dynamic as X (TimeDynamic)
import GHC.Base as X (seq)
import GHC.Enum as X (Bounded (maxBound, minBound))
import GHC.Err as X (error, undefined)
import GHC.Float as X (Double)
import GHC.Generics as X (Generic)
import GHC.Int as X (Int)
import GHC.Integer as X (Integer)
import GHC.Natural as X (Natural)
import GHC.Num as X (Num (fromInteger, (*), (+), (-)))
import GHC.Real as X (Integral (div), fromIntegral)
import GHC.Show as X (Show (show))
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
  ( DecodeTOML (tomlDecoder),
    TOMLError,
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

-- | 'Text' version of 'P.show'.
showt :: (Show a) => a -> Text
showt = pack . show

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
