module Navi.Effects.MonadNotify
  ( MonadNotify (..),
  )
where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans (..))
import DBus.Client (Client)
import DBus.Notify (Note)
import DBus.Notify qualified as DBusN
import Navi.Prelude
import UnexceptionalIO (SomeNonPseudoException)
import UnexceptionalIO qualified

class Monad m => MonadNotify m where
  initConn :: m (Either SomeNonPseudoException Client)
  sendNote :: Client -> Note -> m ()

instance MonadNotify IO where
  initConn = UnexceptionalIO.fromIO DBusN.connectSession
  sendNote client = void . DBusN.notify client

instance MonadNotify m => MonadNotify (ReaderT e m) where
  initConn = lift initConn
  sendNote client = lift . sendNote client
