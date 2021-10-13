module Navi.MonadNavi
  ( Ref,
    MutRef (..),
    MonadFS (..),
    MonadNavi,
  )
where

import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Navi.Prelude
import System.Process qualified as P
import UnexceptionalIO (SomeNonPseudoException)
import UnexceptionalIO qualified

type Ref :: (Type -> Type) -> (Type -> Type)
type family Ref m

class Monad m => MutRef m where
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()

type instance Ref IO = IORef

instance MutRef IO where
  newRef = IORef.newIORef
  readRef = IORef.readIORef
  writeRef = IORef.writeIORef

class Monad m => MonadFS m where
  execSh :: Text -> m Text
  readFile :: FilePath -> m (Either SomeNonPseudoException Text)

instance MonadFS IO where
  execSh = execIO
  readFile = readFileIO

type MonadNavi m = (MonadFS m, MutRef m)

execIO :: Text -> IO Text
execIO cmd = T.pack <$> P.readCreateProcess process ""
  where
    process = P.shell $ T.unpack cmd

readFileIO :: FilePath -> IO (Either SomeNonPseudoException Text)
readFileIO = (<<$>>) T.pack . UnexceptionalIO.fromIO . readFile'
