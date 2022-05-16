-- | Provides an effect for sending system notifications.
module Navi.Effects.MonadNotify
  ( MonadNotify (..),
    sendNoteQueue,
  )
where

import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects.MonadQueue (MonadQueue (..))
import Navi.Env.Core (HasNoteQueue (..))
import Navi.Prelude

-- | This class represents sending desktop notifications.
class Monad m => MonadNotify m where
  sendNote :: NaviNote -> m ()

instance MonadNotify m => MonadNotify (ReaderT e m) where
  sendNote = lift . sendNote
  {-# INLINEABLE sendNote #-}

-- | Convenience function for retrieving a 'Navi.Data.NaviQueue.NaviQueue'
-- 'NaviNote' from the @env@ and sending the note.
sendNoteQueue ::
  (HasNoteQueue env, MonadQueue m, MonadReader env m) =>
  NaviNote ->
  m ()
sendNoteQueue naviNote =
  asks getNoteQueue >>= (`writeQueue` naviNote)
{-# INLINEABLE sendNoteQueue #-}
