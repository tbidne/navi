{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides environment for usage with NotifySend.
module Navi.Env.NotifySend
  ( NotifySendEnv (..),
    mkNotifySendEnv,
    naviToNotifySend,
  )
where

import DBus.Notify (UrgencyLevel (Critical, Low, Normal))
import Navi.Config.Types (Config)
import Navi.Data.NaviLog (LogEnv)
import Navi.Data.NaviNote (NaviNote, Timeout (Never, Seconds))
import Navi.Env.Core
  ( Env (MkEnv),
    HasEvents (..),
    HasLogEnv (..),
    HasLogQueue (..),
    HasNoteQueue (..),
  )
import Navi.Prelude

-- | Concrete notify-send environment. Adds the dbus client.
newtype NotifySendEnv = MkNotifySendEnv
  { coreEnv :: Env
  }

makeFieldLabelsNoPrefix ''NotifySendEnv

instance HasEvents NotifySendEnv where
  getEvents = view (#coreEnv % #events)

instance HasLogEnv NotifySendEnv where
  getLogEnv = view (#coreEnv % #logEnv)
  localLogEnv = over' (#coreEnv % #logEnv)

instance HasLogQueue NotifySendEnv where
  getLogQueue = view (#coreEnv % #logQueue)

instance HasNoteQueue NotifySendEnv where
  getNoteQueue = view (#coreEnv % #noteQueue)

-- | Creates a 'NotifySendEnv' from the provided log types and configuration
-- data.
mkNotifySendEnv ::
  (MonadSTM m) =>
  LogEnv ->
  Config ->
  m NotifySendEnv
mkNotifySendEnv logEnv config = do
  logQueue <- newTBQueueA 1000
  noteQueue <- newTBQueueA 1000
  pure
    $ MkNotifySendEnv
      { coreEnv =
          MkEnv
            (config ^. #events)
            logEnv
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
        <> naviNote
        ^. #summary
        <> "\" "
        <> maybe "" (\b -> " \"" <> b <> "\" ") (naviNote ^. #body)
        <> maybe "" ulToNS (naviNote ^. #urgency)
        <> maybe "" timeoutToNS (naviNote ^. #timeout)

    ulToNS Low = " --urgency low "
    ulToNS Normal = " --urgency normal "
    ulToNS Critical = " --urgency critical "

    timeoutToNS Never = ""
    timeoutToNS (Seconds s) =
      pack
        $ " --expire-time "
        <> show (s * 1_000)
        <> " "
