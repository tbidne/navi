-- | Provides an effect for sending system notifications.
module Navi.Effects.MonadNotify
  ( MonadNotify (..),
  )
where

import Navi.Data.NaviNote (NaviNote (..))
import Navi.Prelude

-- | This class represents sending desktop notifications.
class Monad m => MonadNotify m where
  sendNote :: NaviNote -> m ()

instance MonadNotify m => MonadNotify (ReaderT e m) where
  sendNote = lift . sendNote
