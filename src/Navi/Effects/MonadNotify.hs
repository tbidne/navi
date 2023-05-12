-- | Provides an effect for sending system notifications.
module Navi.Effects.MonadNotify
  ( MonadNotify (..),
    sendNoteQueue,
  )
where

import Navi.Data.NaviNote (NaviNote (..))
import Navi.Env.Core (HasNoteQueue (..))
import Navi.Prelude

{- HLINT ignore MonadNotify "Redundant bracket" -}

-- | This class represents sending desktop notifications.
class (Monad m) => MonadNotify m where
  sendNote :: (HasCallStack) => NaviNote -> m ()

instance (MonadNotify m) => MonadNotify (ReaderT e m) where
  sendNote = lift . sendNote
  {-# INLINEABLE sendNote #-}

-- | Convenience function for retrieving a 'TBQueue'
-- 'NaviNote' from the @env@ and sending the note.
sendNoteQueue ::
  ( HasCallStack,
    HasNoteQueue env,
    MonadReader env m,
    MonadSTM m
  ) =>
  NaviNote ->
  m ()
sendNoteQueue naviNote =
  asks getNoteQueue >>= (`writeTBQueueA` naviNote)
{-# INLINEABLE sendNoteQueue #-}
