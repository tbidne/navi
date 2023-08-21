{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides environment for usage with DBus.
module Navi.Env.DBus
  ( HasDBusClient (..),
    DBusEnv (..),
    mkDBusEnv,
    naviToDBus,
  )
where

import DBus.Client (Client)
import DBus.Notify (Hint (Urgency), Note)
import DBus.Notify qualified as DBusN
import Navi.Config (Config)
import Navi.Data.NaviLog (LogEnv)
import Navi.Data.NaviNote (NaviNote (..), Timeout (..))
import Navi.Env.Core
  ( Env (MkEnv),
    HasEvents (..),
    HasLogEnv (..),
    HasLogQueue (..),
    HasNoteQueue (..),
  )
import Navi.Prelude

-- | Retrieves the notification client.
class HasDBusClient env where
  getClient :: env -> Client

-- | Concrete dbus environment. Adds the dbus client.
data DBusEnv = MkDBusEnv
  { coreEnv :: !Env,
    dbusClient :: !Client
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

-- | Creates a 'DBusEnv' from the provided log types and configuration data.
mkDBusEnv ::
  (HasCallStack, MonadIO m, MonadSTM m) =>
  LogEnv ->
  Config ->
  m DBusEnv
mkDBusEnv logEnv config = do
  client <- liftIO DBusN.connectSession
  logQueue <- newTBQueueA 1000
  noteQueue <- newTBQueueA 1000
  pure
    $ MkDBusEnv
      { coreEnv =
          MkEnv
            (config ^. #events)
            logEnv
            logQueue
            noteQueue,
        dbusClient = client
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
