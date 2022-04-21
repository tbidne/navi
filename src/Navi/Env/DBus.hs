{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides environment for usage with DBus.
module Navi.Env.DBus
  ( HasDBusClient (..),
    DBusEnv (..),
    mkDBusEnv,
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import DBus.Client (Client)
import DBus.Notify qualified as DBusN
import Katip (LogContexts, LogEnv, Namespace)
import Navi.Config (Config)
import Navi.Config qualified as Config
import Navi.Data.NaviQueue (NaviQueue (..))
import Navi.Env.Core
  ( Env (MkEnv),
    HasEvents (..),
    HasLogContexts (..),
    HasLogEnv (..),
    HasLogNamespace (..),
    HasLogQueue (..),
    HasNoteQueue (..),
    HasPollInterval (..),
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

instance HasPollInterval DBusEnv where
  getPollInterval = view (#coreEnv % #pollInterval)

instance HasEvents IORef DBusEnv where
  getEvents = view (#coreEnv % #events)

instance HasLogEnv DBusEnv where
  getLogEnv = view (#coreEnv % #logEnv)
  setLogEnv = set (#coreEnv % #logEnv)
  overLogEnv = over (#coreEnv % #logEnv)

instance HasLogContexts DBusEnv where
  getLogContexts = view (#coreEnv % #logCtx)
  setLogContexts = set (#coreEnv % #logCtx)
  overLogContexts = over (#coreEnv % #logCtx)

instance HasLogNamespace DBusEnv where
  getLogNamespace = view (#coreEnv % #logNamespace)
  setLogNamespace = set (#coreEnv % #logNamespace)
  overLogNamespace = over (#coreEnv % #logNamespace)

instance HasLogQueue DBusEnv where
  getLogQueue = view (#coreEnv % #logQueue)

instance HasNoteQueue DBusEnv where
  getNoteQueue = view (#coreEnv % #noteQueue)

instance HasDBusClient DBusEnv where
  getClient = view #dbusClient

-- | Creates a 'DBusEnv' from the provided log types and configuration data.
mkDBusEnv ::
  MonadIO m =>
  LogEnv ->
  LogContexts ->
  Namespace ->
  Config IORef ->
  m DBusEnv
mkDBusEnv logEnv logContext namespace config = do
  client <- liftIO DBusN.connectSession
  logQueue <- liftIO $ STM.atomically $ TBQueue.newTBQueue 1000
  noteQueue <- liftIO $ STM.atomically $ TBQueue.newTBQueue 1000
  pure $
    MkDBusEnv
      { coreEnv =
          MkEnv
            (config ^. #pollInterval)
            (Config.events config)
            logEnv
            logContext
            namespace
            (MkNaviQueue logQueue)
            (MkNaviQueue noteQueue),
        dbusClient = client
      }
