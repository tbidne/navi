-- | This module provides the core application type and logic.
module Navi
  ( -- * Entry point
    runNavi,

    -- * Application Types
    NaviT (..),
    runNaviT,
  )
where

import Control.Concurrent.Async.Lifted.Safe (Forall, Pure)
import Control.Concurrent.Async.Lifted.Safe qualified as Async
import DBus.Notify (UrgencyLevel (..))
import Data.List.NonEmpty qualified as NE
import Katip (Severity (..))
import Navi.Data.NaviLog (NaviLog (..))
import Navi.Data.NaviNote (NaviNote (..), Timeout (..))
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

-- | Entry point for the application.
runNavi ::
  forall ref env m.
  ( Forall (Pure m),
    HasEvents ref env,
    HasLogNamespace env,
    HasLogQueue env,
    HasNoteQueue env,
    MonadBaseControl IO m,
    MonadCatch m,
    MonadLogger m,
    MonadMutRef ref m,
    MonadNotify m,
    MonadQueue m,
    MonadShell m,
    MonadSystemInfo m,
    MonadReader env m
  ) =>
  m Void
runNavi = do
  let welcome =
        MkNaviNote
          { summary = "Navi",
            body = Just "Navi is up :-)",
            urgency = Just Normal,
            timeout = Just $ Seconds 10
          }
  sendNoteQueue welcome
  events <- asks (getEvents @ref)
  res <- runAllAsync events
  pure $ either (either id id) NE.head res
  where
    -- We use race so that we do not swallow exceptions. If any of these
    -- throw an exception we want to die:
    --
    -- 1. Logging: something is really wrong.
    -- 2. Notify: something is really wrong.
    -- 3. processEvent: should be catching and logging its own exceptions,
    --    so if an exception escapes then something is really wrong.
    runAllAsync evts =
      -- no logging here since logging itself is broken
      pollLogQueue
        -- log and rethrow notify exceptions
        `Async.race` logExAndRethrow "Notify: " pollNoteQueue
        -- log and rethrow anything that escaped the exception handling in
        -- processEvent
        `Async.race` logExAndRethrow
          "Event processing: "
          ( Async.mapConcurrently processEvent evts
          )

    logExAndRethrow prefix io = catchAnyLifted io $ \ex -> do
      sendLogQueue $ MkNaviLog CriticalS (prefix <> pack (displayException ex))
      throwM ex
{-# INLINEABLE runNavi #-}

processEvent ::
  forall m ref env.
  ( HasLogNamespace env,
    HasLogQueue env,
    HasNoteQueue env,
    MonadCatch m,
    MonadLogger m,
    MonadMutRef ref m,
    MonadQueue m,
    MonadReader env m,
    MonadShell m,
    MonadSystemInfo m
  ) =>
  AnyEvent ref ->
  m Void
processEvent (MkAnyEvent event) = addNamespace (fromString $ unpack name) $ do
  let pi = event ^. #pollInterval
  forever $ do
    sendLogQueue $ MkNaviLog InfoS ("Checking " <> name)
    (Event.runEvent event >>= handleSuccess)
      `catch` handleEventError
      `catchAny` handleSomeException
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
{-# INLINEABLE processEvent #-}

eventErrToNote :: EventError -> NaviNote
eventErrToNote ex =
  MkNaviNote
    { summary = ex ^. #name,
      body = Just $ ex ^. #short,
      urgency = Just Critical,
      timeout = Nothing
    }
{-# INLINEABLE eventErrToNote #-}

exToNote :: SomeException -> NaviNote
exToNote ex =
  MkNaviNote
    { summary = "Exception",
      body = Just $ pack (displayException ex),
      urgency = Just Critical,
      timeout = Nothing
    }
{-# INLINEABLE exToNote #-}

pollNoteQueue ::
  ( HasNoteQueue env,
    MonadLogger m,
    MonadNotify m,
    MonadQueue m,
    MonadReader env m
  ) =>
  m Void
pollNoteQueue = addNamespace "note-poller" $ do
  queue <- asks getNoteQueue
  forever $ readQueue queue >>= sendNote
{-# INLINEABLE pollNoteQueue #-}

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
{-# INLINEABLE pollLogQueue #-}
