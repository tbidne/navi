{-# LANGUAGE TemplateHaskell #-}

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
import Effects.MonadAsync qualified as Async
import Effects.MonadLoggerNamespace
  ( MonadLoggerNamespace,
    addNamespace,
    logStrToBs,
  )
import Effects.MonadTerminal (MonadTerminal (putBinary))
import Effects.MonadThread (sleep)
import Navi.Data.NaviNote (NaviNote (..), Timeout (..))
import Navi.Effects.MonadNotify (MonadNotify (..), sendNoteQueue)
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (..))
import Navi.Env.Core
  ( HasEvents (..),
    HasLogEnv (getLogEnv),
    HasLogQueue (..),
    HasNoteQueue (..),
  )
import Navi.Event qualified as Event
import Navi.Event.Types (AnyEvent (..), EventError (..))
import Navi.NaviT (NaviT (..), runNaviT)
import Navi.Prelude

-- | Entry point for the application.
runNavi ::
  forall env m.
  ( HasCallStack,
    HasEvents env,
    HasLogEnv env,
    HasLogQueue env,
    HasNoteQueue env,
    MonadAsync m,
    MonadCallStack m,
    MonadCatch m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadLoggerNamespace m,
    MonadNotify m,
    MonadSTM m,
    MonadSystemInfo m,
    MonadTerminal m,
    MonadThread m,
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
  events <- asks getEvents
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

    logExAndRethrow prefix io = catchAny io $ \ex -> do
      $(logError) (prefix <> pack (displayCallStack ex))
      throwWithCallStack ex
{-# INLINEABLE runNavi #-}

processEvent ::
  forall m env.
  ( HasNoteQueue env,
    MonadCatch m,
    MonadIORef m,
    MonadLoggerNamespace m,
    MonadReader env m,
    MonadSTM m,
    MonadSystemInfo m,
    MonadThread m
  ) =>
  AnyEvent ->
  m Void
processEvent (MkAnyEvent event) = addNamespace (fromString $ unpack name) $ do
  let pi = event ^. (#pollInterval % #unPollInterval)
  forever $ do
    $(logInfo) ("Checking " <> name)
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
          $(logDebug) ("No alert to raise " <> showt result)
          Event.updatePrevTrigger repeatEvent result
        Just note -> do
          blocked <- Event.blockRepeat repeatEvent result
          if blocked
            then $(logDebug) ("Alert blocked " <> showt result)
            else do
              $(logInfo) ("Sending note " <> showt note)
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
      $(logError) (pack $ displayException e)
      if blockErrEvent
        then $(logDebug) "Error note blocked"
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
    MonadLoggerNamespace m,
    MonadNotify m,
    MonadReader env m,
    MonadSTM m
  ) =>
  m Void
pollNoteQueue = addNamespace "note-poller" $ do
  queue <- asks getNoteQueue
  forever $ readTBQueueM queue >>= sendNote
{-# INLINEABLE pollNoteQueue #-}

pollLogQueue ::
  ( HasLogQueue env,
    HasLogEnv env,
    MonadLoggerNamespace m,
    MonadHandleWriter m,
    MonadReader env m,
    MonadSTM m,
    MonadTerminal m
  ) =>
  m Void
pollLogQueue = addNamespace "logger" $ do
  queue <- asks getLogQueue
  mfileHandle <- asks (preview (#logFile % _Just % #handle) . getLogEnv)
  let sendFn = maybe putBinary toFile mfileHandle
  forever $ do
    logStr <- logStrToBs <$> readTBQueueM queue
    sendFn logStr
  where
    toFile h bs = hPut h bs *> hFlush h
{-# INLINEABLE pollLogQueue #-}
