{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides environment for usage with DBus.
module Navi.Env.DBus
  ( HasDBusClient (..),
    DBusEnv (..),
    MonadDBus (..),
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
import Navi.Env.Core
  ( Env (MkEnv),
    HasEvents (getEvents),
    HasLogEnv (getLogEnv, localLogEnv),
    HasLogQueue (getLogQueue),
    HasNoteQueue (getNoteQueue),
  )
import Navi.Prelude

-- | Retrieves the notification client.
class HasDBusClient env where
  getClient :: env -> Client

-- | Concrete dbus environment. Adds the dbus client.
data DBusEnv = MkDBusEnv
  { coreEnv :: Env,
    dbusClient :: Client
  }

makeFieldLabelsNoPrefix ''DBusEnv

instance HasEvents DBusEnv where
  getEvents = view (#coreEnv % #events)

instance HasLogEnv DBusEnv where
  getLogEnv = view (#coreEnv % #logEnv)
  localLogEnv = over' (#coreEnv % #logEnv)

instance HasLogQueue DBusEnv where
  getLogQueue = view (#coreEnv % #logQueue)

instance HasNoteQueue DBusEnv where
  getNoteQueue = view (#coreEnv % #noteQueue)

instance HasDBusClient DBusEnv where
  getClient = view #dbusClient

instance
  ( k ~ A_Lens,
    x ~ Namespace,
    y ~ Namespace
  ) =>
  LabelOptic "namespace" k DBusEnv DBusEnv x y
  where
  labelOptic =
    lensVL $ \f (MkDBusEnv a1 a2) ->
      fmap
        (\b -> MkDBusEnv (set' #namespace b a1) a2)
        (f (a1 ^. #namespace))
  {-# INLINE labelOptic #-}

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
  LogEnv ->
  Config ->
  m Env
mkDBusEnv logEnv config = do
  client <- connectSession
  logQueue <- newTBQueueA 1000
  noteQueue <- newTBQueueA 1000
  pure
    $ MkEnv
      (config ^. #events)
      logEnv
      logQueue
      noteQueue
      (DBus client)
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
