-- | Provides an effect for mutable references.
module Navi.Effects.MonadMutRef
  ( MonadMutRef (..),
  )
where

import Data.IORef qualified as IORef
import Navi.Prelude

-- | Interface for mutable references. This will likely be 'IORef' in 'IO'
-- or some mock type in pure code.
class Monad m => MonadMutRef m ref where
  newRef :: a -> m (ref a)
  readRef :: ref a -> m a
  writeRef :: ref a -> a -> m ()

instance MonadMutRef IO IORef where
  newRef = IORef.newIORef
  readRef = IORef.readIORef
  writeRef = IORef.writeIORef

instance MonadMutRef m ref => MonadMutRef (ReaderT e m) ref where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref
