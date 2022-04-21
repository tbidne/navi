{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides queue functionality.
module Navi.Data.NaviQueue
  ( NaviQueue (..),
    readQueueIO,
    writeQueueIO,
    flushQueueIO,
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue (TBQueue)
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Navi.Prelude

-- | 'NaviQueue' provides a simple api for queue operations. This is
-- intended to be used with concurrency.
newtype NaviQueue a = MkNaviQueue {unNaviQueue :: TBQueue a}

makeFieldLabelsNoPrefix ''NaviQueue

-- | Atomically reads from the queue. Blocks until a value is available.
readQueueIO :: MonadIO m => NaviQueue a -> m a
readQueueIO = liftIO . STM.atomically . TBQueue.readTBQueue . view #unNaviQueue

-- | Atomically writes to the queue.
writeQueueIO :: MonadIO m => NaviQueue a -> a -> m ()
writeQueueIO queue = liftIO . STM.atomically . TBQueue.writeTBQueue (queue ^. #unNaviQueue)

-- | Atomically reads from the queue. Does not retry.
flushQueueIO :: MonadIO m => NaviQueue a -> m [a]
flushQueueIO = liftIO . STM.atomically . STM.flushTBQueue . view #unNaviQueue
