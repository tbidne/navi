-- | Provides the 'MonadQueue' effect.
module Navi.Effects.MonadQueue
  ( MonadQueue (..),
  )
where

import Navi.Prelude
import UnliftIO.STM (atomically, flushTBQueue, readTBQueue, writeTBQueue)

-- | Interface for queue operations.
class Monad m => MonadQueue m where
  -- | Attempts to read from a queue.
  readQueue :: TBQueue a -> m a

  -- | Writes to a queue.
  writeQueue :: TBQueue a -> a -> m ()

  -- | Flushes the queue.
  flushQueue :: TBQueue a -> m [a]

instance MonadQueue IO where
  readQueue = atomically . readTBQueue
  {-# INLINEABLE readQueue #-}
  writeQueue queue = atomically . writeTBQueue queue
  {-# INLINEABLE writeQueue #-}
  flushQueue = atomically . flushTBQueue
  {-# INLINEABLE flushQueue #-}

instance MonadQueue m => MonadQueue (ReaderT e m) where
  readQueue = lift . readQueue
  {-# INLINEABLE readQueue #-}
  writeQueue q = lift . writeQueue q
  {-# INLINEABLE writeQueue #-}
  flushQueue = lift . flushQueue
  {-# INLINEABLE flushQueue #-}
