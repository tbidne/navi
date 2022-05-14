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
  writeQueue = Queue.writeQueueIO
  flushQueue = Queue.flushQueueIO

instance MonadQueue m => MonadQueue (ReaderT e m) where
  readQueue = lift . readQueue
  writeQueue q = lift . writeQueue q
  flushQueue = lift . flushQueue
