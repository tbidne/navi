-- | Provides an effect for sending system notifications.
module Navi.Effects.MonadNotify
  ( NotifySystem (..),
    MonadNotify (..),
  )
where

import Navi.Data.NaviNote (NaviNote (..))
import Navi.Prelude

-- | This class represents sending desktop notifications.
class Monad m => MonadNotify m where
  sendNote :: NaviNote -> m ()

instance MonadNotify m => MonadNotify (ReaderT e m) where
  sendNote = lift . sendNote

-- | Describes the possible notification systems. This type is intended to be
-- used phantom to allow for different typeclass dispatch.
data NotifySystem
  = -- | DBus notifcation system.
    DBus
