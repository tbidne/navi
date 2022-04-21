-- | This module provides the core application type and logic.
module Navi
  ( -- * Entry point
    runNavi,

    -- * Application Types
    NaviT (..),
    runNaviT,
  )
where

import DBus.Notify (UrgencyLevel (..))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Katip (Severity (..))
import Navi.Data.NaviLog (NaviLog (..))
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects.MonadLogger (MonadLogger (..), sendLogQueue)
import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Effects.MonadNotify (MonadNotify (..), sendNoteQueue)
import Navi.Effects.MonadQueue (MonadQueue (..))
import Navi.Effects.MonadQueue qualified as Queue
import Navi.Effects.MonadShell (MonadShell (..))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (..))
import Navi.Env.Core
  ( HasEvents (..),
    HasLogQueue (..),
    HasNoteQueue (..),
    HasPollInterval (..),
  )
import Navi.Event qualified as Event
import Navi.Event.Types (AnyEvent (..), EventErr (..))
import Navi.NaviT (NaviT (..), runNaviT)
import Navi.Prelude
import UnliftIO.Async qualified as Async

-- | Entry point for the application.
runNavi ::
  forall ref env m.
  ( HasEvents ref env,
    HasLogQueue env,
    HasNoteQueue env,
    HasPollInterval env,
    MonadLogger m,
    MonadMutRef ref m,
    MonadNotify m,
    MonadQueue m,
    MonadShell m,
    MonadSystemInfo m,
    MonadReader env m,
    MonadUnliftIO m
  ) =>
  m Void
runNavi = do
  events <- asks (getEvents @ref)
  Async.withAsync pollLogQueue $ \logThread ->
    Async.withAsync pollNoteQueue $ \noteThread ->
      NE.head <$> Async.mapConcurrently processEvent events
        `finally` finishNotify noteThread
        `finally` finishLogging logThread
  where
    finishLogging t = Async.cancel t *> flushLogQueue
    finishNotify t = Async.cancel t *> flushNoteQueue

processEvent ::
  forall m ref env.
  ( HasLogQueue env,
    HasNoteQueue env,
    MonadLogger m,
    MonadMutRef ref m,
    MonadQueue m,
    MonadReader env m,
    MonadShell m,
    MonadSystemInfo m,
    MonadUnliftIO m
  ) =>
  AnyEvent ref ->
  m Void
processEvent (MkAnyEvent event) = do
  let pi = event ^. #pollInterval

  forever $ do
    sendLogQueue $ MkNaviLog DebugS ("Checking " <> event ^. #name)
    (Event.runEvent event >>= handleSuccess)
      `catch` handleEventErr
      `catch` handleSomeException
    sleep pi
  where
    name = event ^. #name
    repeatEvent = event ^. #repeatEvent
    errorNote = event ^. #errorNote
    raiseAlert = event ^. #raiseAlert

    handleSuccess result = addNamespace "Handling Success" $ do
      case raiseAlert result of
        Nothing -> do
          sendLogQueue $ MkNaviLog DebugS (mkLog "No alert to raise" result)
          Event.updatePrevTrigger repeatEvent result
        Just note -> do
          blocked <- Event.blockRepeat repeatEvent result
          if blocked
            then sendLogQueue $ MkNaviLog DebugS (mkLog "Alert blocked" result)
            else do
              sendLogQueue $ MkNaviLog InfoS (mkLog "Sending note" note)
              sendLogQueue $ MkNaviLog InfoS (mkLog "Sending alert" result)
              Event.updatePrevTrigger repeatEvent result
              sendNoteQueue note

    handleEventErr =
      addNamespace "Handling EventErr"
        . handleErr eventErrToNote

    handleSomeException =
      addNamespace "Handling SomeException"
        . handleErr exToNote

    handleErr :: Exception e => (e -> NaviNote) -> e -> m ()
    handleErr toNote e = do
      blockErrEvent <- Event.blockErr errorNote
      sendLogQueue $ MkNaviLog ErrorS (T.pack $ displayException e)
      if blockErrEvent
        then sendLogQueue $ MkNaviLog DebugS "Error note blocked"
        else sendNoteQueue (toNote e)

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

pollNoteQueue ::
  ( HasNoteQueue env,
    HasPollInterval env,
    MonadNotify m,
    MonadQueue m,
    MonadReader env m,
    MonadShell m
  ) =>
  m Void
pollNoteQueue = do
  pi <- asks getPollInterval
  asks getNoteQueue >>= Queue.pollQueueAction pi sendNote

flushNoteQueue ::
  (HasNoteQueue env, MonadNotify m, MonadQueue m, MonadReader env m) =>
  m ()
flushNoteQueue =
  asks getNoteQueue >>= Queue.flushQueueAction sendNote

pollLogQueue ::
  ( HasLogQueue env,
    HasPollInterval env,
    MonadLogger m,
    MonadQueue m,
    MonadReader env m,
    MonadShell m
  ) =>
  m Void
pollLogQueue = do
  pi <- asks getPollInterval
  asks getLogQueue >>= Queue.pollQueueAction pi logText

flushLogQueue ::
  (HasLogQueue env, MonadLogger m, MonadQueue m, MonadReader env m) =>
  m ()
flushLogQueue =
  asks getLogQueue >>= Queue.flushQueueAction logText
