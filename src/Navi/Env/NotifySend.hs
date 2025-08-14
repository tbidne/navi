{-# LANGUAGE UndecidableInstances #-}

-- | Provides environment for usage with NotifySend.
module Navi.Env.NotifySend
  ( mkNotifySendEnv,
    naviToNotifySend,
  )
where

import DBus.Notify (UrgencyLevel (Critical, Low, Normal))
import Navi.Config.Types (Config, NoteSystem (NotifySend))
import Navi.Data.NaviLog (LogEnv)
import Navi.Data.NaviNote (NaviNote, Timeout (Never, Seconds))
import Navi.Env.Core (Env (MkEnv, events, logEnv, noteQueue, notifySystem))
import Navi.Prelude
import Navi.Utils qualified as Utils

-- | Creates a 'NotifySendEnv' from the provided log types and configuration
-- data.
mkNotifySendEnv ::
  (MonadSTM m) =>
  Maybe LogEnv ->
  Config ->
  m Env
mkNotifySendEnv logEnv config = do
  noteQueue <- newTBQueueA 1000
  pure
    $ MkEnv
      { events = config ^. #events,
        logEnv,
        noteQueue,
        notifySystem = NotifySend
      }
{-# INLINEABLE mkNotifySendEnv #-}

-- | Turns a 'NaviNote' into a string to be sent with the notify-send tool.
naviToNotifySend :: NaviNote -> Text
naviToNotifySend naviNote = txt
  where
    txt =
      mconcat
        [ "notify-send \"",
          summary,
          "\" ",
          body,
          maybe "" ulToNS (naviNote ^. #urgency),
          maybe "" timeoutToNS (naviNote ^. #timeout)
        ]

    summary = Utils.escapeDoubleQuotes $ naviNote ^. #summary
    body =
      maybe "" ((\b -> " \"" <> b <> "\" ") . Utils.escapeDoubleQuotes)
        $ naviNote
        ^. #body

    ulToNS Low = " --urgency low "
    ulToNS Normal = " --urgency normal "
    ulToNS Critical = " --urgency critical "

    timeoutToNS Never = ""
    timeoutToNS (Seconds s) =
      packText
        $ " --expire-time "
        <> show (s * 1_000)
        <> " "
