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
import Navi.Config.Types (Config)
import Navi.Data.NaviLog (LogEnv)
import Navi.Data.NaviNote (NaviNote, Timeout (..))
import Navi.Effects.MonadLoggerContext (Namespace)
import Navi.Env.Core
  ( Env (MkEnv),
    HasEvents (..),
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
  {-# INLINEABLE getEvents #-}

instance HasLogEnv NotifySendEnv where
  getLogEnv = view (#coreEnv % #logEnv)
  {-# INLINEABLE getLogEnv #-}
  localLogEnv = over' (#coreEnv % #logEnv)
  {-# INLINEABLE localLogEnv #-}

instance HasLogNamespace NotifySendEnv where
  getLogNamespace = view (#coreEnv % #logNamespace)
  {-# INLINEABLE getLogNamespace #-}
  localLogNamespace = over' (#coreEnv % #logNamespace)
  {-# INLINEABLE localLogNamespace #-}

instance HasLogQueue NotifySendEnv where
  getLogQueue = view (#coreEnv % #logQueue)
  {-# INLINEABLE getLogQueue #-}

instance HasNoteQueue NotifySendEnv where
  getNoteQueue = view (#coreEnv % #noteQueue)
  {-# INLINEABLE getNoteQueue #-}

-- | Creates a 'NotifySendEnv' from the provided log types and configuration
-- data.
mkNotifySendEnv ::
  MonadIO m =>
  LogEnv ->
  Namespace ->
  Config IORef ->
  m NotifySendEnv
mkNotifySendEnv logEnv namespace config = do
  logQueue <- liftIO $ STM.atomically $ TBQueue.newTBQueue 1000
  noteQueue <- liftIO $ STM.atomically $ TBQueue.newTBQueue 1000
  pure $
    MkNotifySendEnv
      { coreEnv =
          MkEnv
            (config ^. #events)
            logEnv
            namespace
            logQueue
            noteQueue
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
{-# INLINEABLE naviToNotifySend #-}
