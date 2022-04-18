-- | This module provides the core application type and logic.
module Navi
  ( -- * Entry point
    runNavi,

    -- * Application Types
    NotifySystem (..),
    NaviT (..),
    runNaviT,
  )
where

import DBus.Notify (UrgencyLevel (..))
import Data.Text qualified as T
import Katip (Severity (..))
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects.MonadLogger (MonadLogger (..))
import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Effects.MonadNotify (MonadNotify (..), NotifySystem (..))
import Navi.Effects.MonadShell (MonadShell (..))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (..))
import Navi.Env.Core (HasEvents (..), HasPollInterval (..))
import Navi.Event qualified as Event
import Navi.Event.Types (AnyEvent (..), EventErr (..))
import Navi.NaviT (NaviT (..), runNaviT)
import Navi.Prelude

-- | Entry point for the application.
runNavi ::
  forall ref env m.
  ( HasEvents ref env,
    HasPollInterval env,
    MonadCatch m,
    MonadLogger m,
    MonadMutRef m ref,
    MonadNotify m,
    MonadShell m,
    MonadSystemInfo m,
    MonadReader env m
  ) =>
  m Void
runNavi = do
  pollInterval <- asks getPollInterval
  events <- asks (getEvents @ref)
  forever $ do
    sleep pollInterval
    logText DebugS "Checking alerts..."
    traverse processEvent events

processEvent ::
  forall m ref.
  ( MonadCatch m,
    MonadLogger m,
    MonadMutRef m ref,
    MonadNotify m,
    MonadSystemInfo m
  ) =>
  AnyEvent ref ->
  m ()
processEvent (MkAnyEvent event) = do
  logText DebugS $ "Checking " <> event ^. #name
  (Event.runEvent event >>= handleSuccess)
    `catch` handleEventErr
    `catch` handleSomeException
  where
    name = event ^. #name
    repeatEvent = event ^. #repeatEvent
    errorNote = event ^. #errorNote
    raiseAlert = event ^. #raiseAlert

    handleSuccess result = addNamespace "Handling Success" $ do
      case raiseAlert result of
        Nothing -> do
          logText DebugS $ mkLog "No alert to raise" result
          Event.updatePrevTrigger repeatEvent result
        Just note -> do
          blocked <- Event.blockRepeat repeatEvent result
          if blocked
            then logText DebugS $ mkLog "Alert blocked" result
            else do
              logText InfoS $ mkLog "Sending note" note
              logText InfoS $ mkLog "Sending alert" result
              Event.updatePrevTrigger repeatEvent result
              sendNote note

    handleEventErr =
      addNamespace "Handling EventErr"
        . handleErr eventErrToNote

    handleSomeException =
      addNamespace "Handling SomeException"
        . handleErr exToNote

    handleErr :: Exception e => (e -> NaviNote) -> e -> m ()
    handleErr toNote e = do
      blockErrEvent <- Event.blockErr errorNote
      logText ErrorS $ T.pack $ displayException e
      if blockErrEvent
        then logText DebugS "Error note blocked"
        else sendNote (toNote e)

    mkLog :: Show a => Text -> a -> Text
    mkLog msg x = "[" <> name <> "] " <> msg <> ": " <> showt x

eventErrToNote :: EventErr -> NaviNote
eventErrToNote ex =
  MkNaviNote
    { summary = ex ^. #name,
      body = Just $ ex ^. #short,
      urgency = Just Critical,
      timeout = Nothing
    }

exToNote :: SomeException -> NaviNote
exToNote ex =
  MkNaviNote
    { summary = "Exception",
      body = Just $ T.pack (displayException ex),
      urgency = Just Critical,
      timeout = Nothing
    }
