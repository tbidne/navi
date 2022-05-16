-- | Provides the 'MonadQueue' effect.
module Navi.Effects.MonadQueue
  ( MonadQueue (..),
  )
where

import Navi.Data.NaviQueue (NaviQueue)
import Navi.Data.NaviQueue qualified as Queue
import Navi.Prelude

-- | Interface for queue operations.
class Monad m => MonadQueue m where
  -- | Attempts to read from a queue.
  readQueue :: NaviQueue a -> m a

  -- | Writes to a queue.
  writeQueue :: NaviQueue a -> a -> m ()

  -- | Flushes the queue.
  flushQueue :: NaviQueue a -> m [a]

instance MonadQueue IO where
  readQueue = Queue.readQueueIO
  {-# INLINEABLE readQueue #-}
  writeQueue = Queue.writeQueueIO
  {-# INLINEABLE writeQueue #-}
  flushQueue = Queue.flushQueueIO
  {-# INLINEABLE flushQueue #-}

instance MonadQueue m => MonadQueue (ReaderT e m) where
  readQueue = lift . readQueue
  {-# INLINEABLE readQueue #-}
  writeQueue q = lift . writeQueue q
  {-# INLINEABLE writeQueue #-}
  flushQueue = lift . flushQueue
  {-# INLINEABLE flushQueue #-}
