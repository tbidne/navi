-- | Provides a \"shell\" effect.
module Navi.Effects.MonadShell
  ( MonadShell (..),
  )
where

import Control.Concurrent qualified as CC
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans (..))
import Data.Text qualified as T
import Navi.Prelude
import Numeric.Data.NonNegative (NonNegative)
import Numeric.Data.NonNegative qualified as NonNegative

-- | This class represents effects that a shell can provide.
class Monad m => MonadShell m where
  readFile :: FilePath -> m (Either SomeException Text)
  sleep :: NonNegative Int -> m ()

instance MonadShell IO where
  readFile = readFileIO
  sleep = CC.threadDelay . (*) 1_000_000 . NonNegative.unNonNegative

instance MonadShell m => MonadShell (ReaderT e m) where
  readFile = lift . readFile
  sleep = lift . sleep

readFileIO :: FilePath -> IO (Either SomeException Text)
readFileIO = (<<$>>) T.pack . try . readFile'
