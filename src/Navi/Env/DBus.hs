{-# LANGUAGE UndecidableInstances #-}

-- | Provides environment for usage with DBus.
module Navi.Env.DBus
  ( MonadDBus (..),
    mkDBusEnv,
    naviToDBus,
  )
where

import DBus.Client (Client)
import DBus.Client qualified as DBus
import DBus.Notify (Hint (Urgency), Note)
import DBus.Notify qualified as DBusN
import Navi.Config (Config)
import Navi.Config.Types (NoteSystem (DBus))
import Navi.Data.NaviLog (LogEnv)
import Navi.Data.NaviNote (NaviNote, Timeout (Never, Seconds))
import Navi.Env.Core (Env (MkEnv, events, logEnv, noteQueue, notifySystem))
import Navi.Prelude

class (Monad m) => MonadDBus m where
  -- | Connects to DBus.
  connectSession :: (HasCallStack) => m Client

  -- | Sends a notification to DBus.
  notify :: (HasCallStack) => Client -> NaviNote -> m ()

instance MonadDBus IO where
  connectSession = DBus.connectSession

  notify client = void . DBusN.notify client . naviToDBus

instance (MonadDBus m) => MonadDBus (ReaderT env m) where
  connectSession = lift connectSession

  notify c = lift . notify c

-- | Creates a 'DBusEnv' from the provided log types and configuration data.
mkDBusEnv ::
  (HasCallStack, MonadDBus m, MonadSTM m) =>
  Maybe LogEnv ->
  Config ->
  m Env
mkDBusEnv logEnv config = do
  client <- connectSession
  noteQueue <- newTBQueueA 1000
  pure
    $ MkEnv
      { events = config ^. #events,
        logEnv,
        noteQueue,
        notifySystem = DBus client
      }
{-# INLINEABLE mkDBusEnv #-}

-- | Turns a 'NaviNote' into a DBus 'Note'.
naviToDBus :: NaviNote -> Note
naviToDBus naviNote =
  DBusN.Note
    { appName = "Navi",
      summary = unpack $ naviNote ^. #summary,
      body = body,
      appImage = Nothing,
      hints = hints,
      expiry = timeout,
      actions = []
    }
  where
    body = DBusN.Text . unpack <$> naviNote ^. #body
    hints = maybeToList $ Urgency <$> naviNote ^. #urgency
    timeout = maybe defTimeout naviToDBusTimeout $ naviNote ^. #timeout
    defTimeout = DBusN.Milliseconds 10_000

naviToDBusTimeout :: Timeout -> DBusN.Timeout
naviToDBusTimeout Never = DBusN.Never
naviToDBusTimeout (Seconds s) = DBusN.Milliseconds $ (* 1_000) $ w16ToInt32 s
  where
    w16ToInt32 :: Word16 -> Int32
    w16ToInt32 = fromIntegral
