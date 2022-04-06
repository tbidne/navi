-- | Provides an effect for sending system notifications.
module Navi.Effects.MonadNotify
  ( MonadNotify (..),
  )
where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans (..))
import DBus.Client (Client)
import DBus.Notify (Note (..))
import DBus.Notify qualified as DBusN
import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote (..), Timeout (..))
import Navi.Prelude
import Numeric.Data.NonNegative qualified as NonNegative
import UnexceptionalIO (SomeNonPseudoException)
import UnexceptionalIO qualified

-- | This class represents sending desktop notifications. For now it is
-- implemented in terms of 'DBus.Client', though this may be generalized
-- to other notification systems.
class Monad m => MonadNotify m where
  initConn :: m (Either SomeNonPseudoException Client)
  sendNote :: Client -> NaviNote -> m ()

instance MonadNotify IO where
  initConn = UnexceptionalIO.fromIO DBusN.connectSession
  sendNote client = void . DBusN.notify client . naviToDBus

instance MonadNotify m => MonadNotify (ReaderT e m) where
  initConn = lift initConn
  sendNote client = lift . sendNote client

naviToDBus :: NaviNote -> Note
naviToDBus naviNote =
  Note
    { appName = "Navi",
      DBusN.summary = T.unpack $ naviNote ^. #summary,
      DBusN.body = body,
      DBusN.appImage = Nothing,
      DBusN.hints = hints,
      DBusN.expiry = timeout,
      DBusN.actions = []
    }
  where
    body = DBusN.Text . T.unpack <$> naviNote ^. #body
    hints = maybeToList $ DBusN.Urgency <$> naviNote ^. #urgency
    timeout = maybe defTimeout naviToDBusTimeout $ naviNote ^. #timeout
    defTimeout = DBusN.Milliseconds 10_000

naviToDBusTimeout :: Timeout -> DBusN.Timeout
naviToDBusTimeout Never = DBusN.Never
naviToDBusTimeout (Seconds s) = DBusN.Milliseconds $ fromIntegral $ multNN s
  where
    multNN = (* 1_000) . NonNegative.unNonNegative
