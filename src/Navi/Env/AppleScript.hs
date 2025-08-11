{-# LANGUAGE UndecidableInstances #-}

-- | Provides environment for usage with AppleScript.
module Navi.Env.AppleScript
  ( mkAppleScriptEnv,
    naviToAppleScript,
  )
where

import Navi.Config.Types (Config, NoteSystem (AppleScript))
import Navi.Data.NaviLog (LogEnv)
import Navi.Data.NaviNote (NaviNote)
import Navi.Env.Core (Env (MkEnv))
import Navi.Prelude

-- | Creates a 'AppleScriptEnv' from the provided log types and configuration
-- data.
mkAppleScriptEnv ::
  (MonadSTM m) =>
  LogEnv ->
  Config ->
  m Env
mkAppleScriptEnv logEnv config = do
  logQueue <- newTBQueueA 1000
  noteQueue <- newTBQueueA 1000
  pure
    $ MkEnv
      (config ^. #events)
      logEnv
      logQueue
      noteQueue
      AppleScript
{-# INLINEABLE mkAppleScriptEnv #-}

-- | Turns a 'NaviNote' into a string to be sent with the notify-send tool.
naviToAppleScript :: NaviNote -> Text
naviToAppleScript naviNote = txt
  where
    txt =
      mconcat
        [ "osascript -e 'display notification ",
          maybe "" withDoubleQuotes (naviNote ^. #body),
          " with title \"Navi\" ",
          " subtitle ",
          withDoubleQuotes (naviNote ^. #summary),
          "'"
        ]

withDoubleQuotes :: Text -> Text
withDoubleQuotes s = " \"" <> s <> "\" "
