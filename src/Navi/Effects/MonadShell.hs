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
import Refined (NonNegative, Refined)
import Refined qualified as R
import UnexceptionalIO (SomeNonPseudoException)
import UnexceptionalIO qualified

-- | This class represents effects that a shell can provide.
class Monad m => MonadShell m where
  readFile :: FilePath -> m (Either SomeNonPseudoException Text)
  sleep :: Refined NonNegative Int -> m ()

instance MonadShell IO where
  readFile = readFileIO
  sleep = CC.threadDelay . (*) 1_000_000 . R.unrefine

instance MonadShell m => MonadShell (ReaderT e m) where
  readFile = lift . readFile
  sleep = lift . sleep

readFileIO :: FilePath -> IO (Either SomeNonPseudoException Text)
readFileIO = (<<$>>) T.pack . UnexceptionalIO.fromIO . readFile'
