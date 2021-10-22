-- | Provides a \"shell\" effect.
module Navi.Effects.MonadShell
  ( MonadShell (..),
  )
where

import Control.Concurrent qualified as CC
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans (..))
import Data.Text qualified as T
import Navi.Data.NonNegative (NonNegative (..))
import Navi.Event.Types (Command (..))
import Navi.Prelude
import System.Process qualified as P
import UnexceptionalIO (SomeNonPseudoException)
import UnexceptionalIO qualified

-- | This class represents effects that a shell can provide.
class Monad m => MonadShell m where
  execSh :: Command -> m (Either SomeNonPseudoException Text)
  readFile :: FilePath -> m (Either SomeNonPseudoException Text)
  sleep :: NonNegative -> m ()

instance MonadShell IO where
  execSh = execIO
  readFile = readFileIO
  sleep = CC.threadDelay . (*) 1_000_000 . unNonNegative

instance MonadShell m => MonadShell (ReaderT e m) where
  execSh = lift . execSh
  readFile = lift . readFile
  sleep = lift . sleep

execIO :: Command -> IO (Either SomeNonPseudoException Text)
execIO (MkCommand cmd) = T.pack <<$>> UnexceptionalIO.fromIO (P.readCreateProcess process "")
  where
    process = P.shell $ T.unpack cmd

readFileIO :: FilePath -> IO (Either SomeNonPseudoException Text)
readFileIO = (<<$>>) T.pack . UnexceptionalIO.fromIO . readFile'
