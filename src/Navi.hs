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
import Katip (Severity (..))
import Navi.Data.NaviLog (NaviLog (..))
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects.MonadLogger (MonadLogger (..), sendLogQueue)
import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Effects.MonadNotify (MonadNotify (..), sendNoteQueue)
import Navi.Effects.MonadQueue (MonadQueue (..))
import Navi.Effects.MonadShell (MonadShell (..))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (..))
import Navi.Env.Core
  ( HasEvents (..),
    HasLogNamespace,
    HasLogQueue (..),
    HasNoteQueue (..),
  )
import Navi.Event qualified as Event
import Navi.Event.Types (AnyEvent (..), EventError (..))
import Navi.NaviT (NaviT (..), runNaviT)
import Navi.Prelude
import UnliftIO.Async qualified as Async

-- | Entry point for the application.
runNavi ::
  forall ref env m.
  ( HasEvents ref env,
    HasLogNamespace env,
    HasLogQueue env,
    HasNoteQueue env,
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
  res <- runAllAsync events
  pure $ either (either id id) NE.head res
  where
    -- We use race so that we do not swallow exceptions. If any of these
    -- throw an exception we want to die:
    -- 1. If logging throws an exception then something is really wrong.
    -- 2. pollNoteQueue should be catching and logging its own exceptions.
    -- 3. processEvent should be catching and logging its own exceptions.
    runAllAsync evts =
      pollLogQueue
        `Async.race` pollNoteQueue
        `Async.race` Async.mapConcurrently processEvent evts

processEvent ::
  forall m ref env.
  ( HasLogNamespace env,
    HasLogQueue env,
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
processEvent (MkAnyEvent event) = addNamespace (fromString $ unpack name) $ do
  let pi = event ^. #pollInterval
  forever $ do
    sendLogQueue $ MkNaviLog DebugS ("Checking " <> name)
    (Event.runEvent event >>= handleSuccess)
      `catch` handleEventError
      `catch` handleSomeException
    sleep pi
  where
    name = event ^. #name
    repeatEvent = event ^. #repeatEvent
    errorNote = event ^. #errorNote
    raiseAlert = event ^. #raiseAlert

    handleSuccess result = addNamespace "handleSuccess" $ do
      case raiseAlert result of
        Nothing -> do
          sendLogQueue $ MkNaviLog DebugS ("No alert to raise " <> showt result)
          Event.updatePrevTrigger repeatEvent result
        Just note -> do
          blocked <- Event.blockRepeat repeatEvent result
          if blocked
            then sendLogQueue $ MkNaviLog DebugS ("Alert blocked " <> showt result)
            else do
              sendLogQueue $ MkNaviLog InfoS ("Sending note " <> showt note)
              Event.updatePrevTrigger repeatEvent result
              sendNoteQueue note

    handleEventError =
      addNamespace "handleEventError"
        . handleErr eventErrToNote

    handleSomeException =
      addNamespace "handleSomeException"
        . handleErr exToNote

    handleErr :: Exception e => (e -> NaviNote) -> e -> m ()
    handleErr toNote e = do
      blockErrEvent <- Event.blockErr errorNote
      sendLogQueue $ MkNaviLog ErrorS (pack $ displayException e)
      if blockErrEvent
        then sendLogQueue $ MkNaviLog DebugS "Error note blocked"
        else sendNoteQueue (toNote e)

eventErrToNote :: EventError -> NaviNote
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
      body = Just $ pack (displayException ex),
      urgency = Just Critical,
      timeout = Nothing
    }

pollNoteQueue ::
  ( HasLogNamespace env,
    HasLogQueue env,
    HasNoteQueue env,
    MonadLogger m,
    MonadNotify m,
    MonadQueue m,
    MonadReader env m,
    MonadUnliftIO m
  ) =>
  m Void
pollNoteQueue = addNamespace "note-poller" $ do
  queue <- asks getNoteQueue
  forever $ do
    (readQueue queue >>= sendNote)
      `catchAny` \ex ->
        sendLogQueue $
          MkNaviLog
            ErrorS
            ( pack $
                "Notification exception: "
                  <> displayException ex
            )

pollLogQueue ::
  ( HasLogQueue env,
    MonadLogger m,
    MonadQueue m,
    MonadReader env m
  ) =>
  m Void
pollLogQueue = addNamespace "logger" $ do
  queue <- asks getLogQueue
  forever $ readQueue queue >>= uncurry logText
