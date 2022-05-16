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

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import DBus.Client (Client)
import DBus.Notify (Hint (Urgency), Note)
import DBus.Notify qualified as DBusN
import Katip (LogContexts, LogEnv, Namespace)
import Navi.Config (Config)
import Navi.Config qualified as Config
import Navi.Data.NaviNote (NaviNote (..), Timeout (..))
import Navi.Data.NaviQueue (NaviQueue (..))
import Navi.Env.Core
  ( Env (MkEnv),
    HasEvents (..),
    HasLogContexts (..),
    HasLogEnv (..),
    HasLogNamespace (..),
    HasLogQueue (..),
    HasNoteQueue (..),
  )
import Navi.Prelude

-- | Retrieves the notification client.
class HasDBusClient env where
  getClient :: env -> Client

-- | Concrete dbus environment. Adds the dbus client.
data DBusEnv = MkDBusEnv
  { coreEnv :: !(Env IORef),
    dbusClient :: !Client
  }

makeFieldLabelsNoPrefix ''DBusEnv

instance HasEvents IORef DBusEnv where
  getEvents = view (#coreEnv % #events)
  {-# INLINEABLE getEvents #-}

instance HasLogEnv DBusEnv where
  getLogEnv = view (#coreEnv % #logEnv)
  {-# INLINEABLE getLogEnv #-}
  setLogEnv = set (#coreEnv % #logEnv)
  {-# INLINEABLE setLogEnv #-}
  overLogEnv = over (#coreEnv % #logEnv)
  {-# INLINEABLE overLogEnv #-}

instance HasLogContexts DBusEnv where
  getLogContexts = view (#coreEnv % #logCtx)
  {-# INLINEABLE getLogContexts #-}
  setLogContexts = set (#coreEnv % #logCtx)
  {-# INLINEABLE setLogContexts #-}
  overLogContexts = over (#coreEnv % #logCtx)
  {-# INLINEABLE overLogContexts #-}

instance HasLogNamespace DBusEnv where
  getLogNamespace = view (#coreEnv % #logNamespace)
  {-# INLINEABLE getLogNamespace #-}
  setLogNamespace = set (#coreEnv % #logNamespace)
  {-# INLINEABLE setLogNamespace #-}
  overLogNamespace = over (#coreEnv % #logNamespace)
  {-# INLINEABLE overLogNamespace #-}

instance HasLogQueue DBusEnv where
  getLogQueue = view (#coreEnv % #logQueue)
  {-# INLINEABLE getLogQueue #-}

instance HasNoteQueue DBusEnv where
  getNoteQueue = view (#coreEnv % #noteQueue)
  {-# INLINEABLE getNoteQueue #-}

instance HasDBusClient DBusEnv where
  getClient = view #dbusClient
  {-# INLINEABLE getClient #-}

-- | Creates a 'DBusEnv' from the provided log types and configuration data.
mkDBusEnv ::
  MonadBase IO m =>
  LogEnv ->
  LogContexts ->
  Namespace ->
  Config IORef ->
  m DBusEnv
mkDBusEnv logEnv logContext namespace config = do
  client <- liftBase DBusN.connectSession
  logQueue <- liftBase $ STM.atomically $ TBQueue.newTBQueue 1000
  noteQueue <- liftBase $ STM.atomically $ TBQueue.newTBQueue 1000
  pure $
    MkDBusEnv
      { coreEnv =
          MkEnv
            (Config.events config)
            logEnv
            logContext
            namespace
            (MkNaviQueue logQueue)
            (MkNaviQueue noteQueue),
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
{-# INLINEABLE naviToDBus #-}

naviToDBusTimeout :: Timeout -> DBusN.Timeout
naviToDBusTimeout Never = DBusN.Never
naviToDBusTimeout (Seconds s) = DBusN.Milliseconds $ (* 1_000) $ w16ToInt32 s
  where
    w16ToInt32 :: Word16 -> Int32
    w16ToInt32 = fromIntegral
{-# INLINEABLE naviToDBusTimeout #-}
