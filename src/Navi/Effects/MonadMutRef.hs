-- | Provides an effect for mutable references.
module Navi.Effects.MonadMutRef
  ( MonadMutRef (..),
  )
where

import Data.IORef qualified as IORef
import Navi.Prelude

-- | Interface for mutable references. This will likely be 'IORef' in 'IO'
-- or some mock type in pure code.
class Monad m => MonadMutRef ref m where
  newRef :: a -> m (ref a)
  readRef :: ref a -> m a
  writeRef :: ref a -> a -> m ()

instance MonadMutRef IORef IO where
  newRef = IORef.newIORef
  readRef = IORef.readIORef
  writeRef = IORef.writeIORef

instance MonadMutRef ref m => MonadMutRef ref (ReaderT e m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

instance (MonadMutRef ref m, Monoid s) => MonadMutRef ref (WriterT s m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
