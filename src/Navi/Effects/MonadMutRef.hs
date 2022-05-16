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
  {-# INLINEABLE newRef #-}
  readRef = IORef.readIORef
  {-# INLINEABLE readRef #-}
  writeRef = IORef.writeIORef
  {-# INLINEABLE writeRef #-}

instance MonadMutRef ref m => MonadMutRef ref (ReaderT e m) where
  newRef = lift . newRef
  {-# INLINEABLE newRef #-}
  readRef = lift . readRef
  {-# INLINEABLE readRef #-}
  writeRef ref = lift . writeRef ref
  {-# INLINEABLE writeRef #-}
