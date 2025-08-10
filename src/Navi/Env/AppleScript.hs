{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides environment for usage with AppleScript.
module Navi.Env.AppleScript
  ( AppleScriptEnv (..),
    mkAppleScriptEnv,
    naviToAppleScript,
  )
where

import Navi.Config.Types (Config)
import Navi.Data.NaviLog (LogEnv)
import Navi.Data.NaviNote (NaviNote)
import Navi.Env.Core
  ( Env (MkEnv),
    HasEvents (..),
    HasLogEnv (..),
    HasLogQueue (..),
    HasNoteQueue (..),
  )
import Navi.Prelude

-- | Concrete notify-send environment.
newtype AppleScriptEnv = MkAppleScriptEnv
  { coreEnv :: Env
  }

makeFieldLabelsNoPrefix ''AppleScriptEnv

instance HasEvents AppleScriptEnv where
  getEvents = view (#coreEnv % #events)

instance HasLogEnv AppleScriptEnv where
  getLogEnv = view (#coreEnv % #logEnv)
  localLogEnv = over' (#coreEnv % #logEnv)

instance HasLogQueue AppleScriptEnv where
  getLogQueue = view (#coreEnv % #logQueue)

instance HasNoteQueue AppleScriptEnv where
  getNoteQueue = view (#coreEnv % #noteQueue)

instance
  ( k ~ A_Lens,
    x ~ Namespace,
    y ~ Namespace
  ) =>
  LabelOptic "namespace" k AppleScriptEnv AppleScriptEnv x y
  where
  labelOptic =
    lensVL $ \f (MkAppleScriptEnv a1) ->
      fmap
        (\b -> MkAppleScriptEnv (set' #namespace b a1))
        (f (a1 ^. #namespace))
  {-# INLINE labelOptic #-}

-- | Creates a 'AppleScriptEnv' from the provided log types and configuration
-- data.
mkAppleScriptEnv ::
  (MonadSTM m) =>
  LogEnv ->
  Config ->
  m AppleScriptEnv
mkAppleScriptEnv logEnv config = do
  logQueue <- newTBQueueA 1000
  noteQueue <- newTBQueueA 1000
  pure
    $ MkAppleScriptEnv
      { coreEnv =
          MkEnv
            (config ^. #events)
            logEnv
            logQueue
            noteQueue
      }
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
