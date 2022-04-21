-- | Provides the 'MonadQueue' effect.
module Navi.Effects.MonadQueue
  ( MonadQueue (..),
    pollQueueAction,
    flushQueueAction,
  )
where

import Navi.Data.NaviQueue (NaviQueue)
import Navi.Data.NaviQueue qualified as Queue
import Navi.Prelude

-- | Interface for queue operations.
class Monad m => MonadQueue m where
  -- | Attempts to read from a queue.
  readQueue :: NaviQueue a -> m (Maybe a)

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

-- | Infinite loop that continuously reads from the queue and applies
-- the given action.
pollQueueAction :: MonadQueue m => (a -> m ()) -> NaviQueue a -> m Void
pollQueueAction action queue =
  forever $ do
    maybeX <- readQueue queue
    case maybeX of
      Nothing -> pure ()
      Just n -> action n

-- | Flushes the queue and applies the action to every item that is flushed.
flushQueueAction :: MonadQueue m => (a -> m ()) -> NaviQueue a -> m ()
flushQueueAction action queue = flushQueue queue >>= traverse_ action
