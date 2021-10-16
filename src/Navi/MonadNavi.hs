module Navi.MonadNavi
  ( MutRef (..),
    MonadNavi (..),
  )
where

import DBus.Client (Client)
import DBus.Notify (Note)
import Navi.Data.NonNegative (NonNegative)
import Navi.Prelude
import UnexceptionalIO (SomeNonPseudoException)

class Monad m => MutRef m where
  type Ref m :: Type -> Type
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()

class MutRef m => MonadNavi m where
  connectDBus :: m (Either SomeNonPseudoException Client)
  execSh :: Text -> m (Either SomeNonPseudoException Text)
  readFile :: FilePath -> m (Either SomeNonPseudoException Text)
  sleep :: NonNegative -> m ()
  sendNote :: Client -> Note -> m ()
