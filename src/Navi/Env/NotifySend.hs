{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides environment for usage with NotifySend.
module Navi.Env.NotifySend
  ( NotifySendEnv (..),
    mkNotifySendEnv,
    naviToNotifySend,
  )
where

import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import DBus.Notify (UrgencyLevel (..))
import Katip (LogContexts, LogEnv, Namespace)
import Navi.Config (Config)
import Navi.Config qualified as Config
import Navi.Data.NaviNote (NaviNote, Timeout (..))
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

-- | Concrete notify-send environment. Adds the dbus client.
newtype NotifySendEnv = MkNotifySendEnv
  { coreEnv :: Env IORef
  }

makeFieldLabelsNoPrefix ''NotifySendEnv

instance HasEvents IORef NotifySendEnv where
  getEvents = view (#coreEnv % #events)

instance HasLogEnv NotifySendEnv where
  getLogEnv = view (#coreEnv % #logEnv)
  setLogEnv = set (#coreEnv % #logEnv)
  overLogEnv = over (#coreEnv % #logEnv)

instance HasLogContexts NotifySendEnv where
  getLogContexts = view (#coreEnv % #logCtx)
  setLogContexts = set (#coreEnv % #logCtx)
  overLogContexts = over (#coreEnv % #logCtx)

instance HasLogNamespace NotifySendEnv where
  getLogNamespace = view (#coreEnv % #logNamespace)
  setLogNamespace = set (#coreEnv % #logNamespace)
  overLogNamespace = over (#coreEnv % #logNamespace)

instance HasLogQueue NotifySendEnv where
  getLogQueue = view (#coreEnv % #logQueue)

instance HasNoteQueue NotifySendEnv where
  getNoteQueue = view (#coreEnv % #noteQueue)

-- | Creates a 'NotifySendEnv' from the provided log types and configuration
-- data.
mkNotifySendEnv ::
  MonadIO m =>
  LogEnv ->
  LogContexts ->
  Namespace ->
  Config IORef ->
  m NotifySendEnv
mkNotifySendEnv logEnv logContext namespace config = do
  logQueue <- liftIO $ STM.atomically $ TBQueue.newTBQueue 1000
  noteQueue <- liftIO $ STM.atomically $ TBQueue.newTBQueue 1000
  pure $
    MkNotifySendEnv
      { coreEnv =
          MkEnv
            (Config.events config)
            logEnv
            logContext
            namespace
            (MkNaviQueue logQueue)
            (MkNaviQueue noteQueue)
      }
{-# INLINEABLE mkNotifySendEnv #-}

-- | Turns a 'NaviNote' into a string to be sent with the notify-send tool.
naviToNotifySend :: NaviNote -> Text
naviToNotifySend naviNote = txt
  where
    txt =
      "notify-send \""
        <> naviNote ^. #summary
        <> "\" "
        <> maybe "" (\b -> " \"" <> b <> "\" ") (naviNote ^. #body)
        <> maybe "" ulToNS (naviNote ^. #urgency)
        <> maybe "" timeoutToNS (naviNote ^. #timeout)

    ulToNS Low = " --urgency low "
    ulToNS Normal = " --urgency normal "
    ulToNS Critical = " --urgency critical "

    timeoutToNS Never = ""
    timeoutToNS (Seconds s) =
      pack $
        " --expire-time "
          <> show (s * 1_000)
          <> " "
