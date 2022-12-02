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
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import Effects.MonadLoggerNamespace
  ( MonadLoggerNamespace,
    addNamespace,
    logStrToBs,
  )
import Effects.MonadThread (sleep)
import Navi.Data.NaviNote (NaviNote (..), Timeout (..))
import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Effects.MonadNotify (MonadNotify (..), sendNoteQueue)
import Navi.Effects.MonadQueue (MonadQueue (..))
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
import UnliftIO.Async qualified as Async

-- | Entry point for the application.
runNavi ::
  forall ref env m.
  ( HasCallStack,
    HasEvents ref env,
    HasLogEnv env,
    HasLogQueue env,
    HasNoteQueue env,
    MonadCallStack m,
    MonadLoggerNamespace m,
    MonadMutRef ref m,
    MonadNotify m,
    MonadQueue m,
    MonadThread m,
    MonadSystemInfo m,
    MonadReader env m,
    MonadUnliftIO m
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

    logExAndRethrow prefix io = catchAny io $ \ex -> do
      $(logError) (prefix <> pack (prettyAnnotated ex))
      throwWithCallStack ex
{-# INLINEABLE runNavi #-}

processEvent ::
  forall m ref env.
  ( HasNoteQueue env,
    MonadLoggerNamespace m,
    MonadMutRef ref m,
    MonadQueue m,
    MonadReader env m,
    MonadSystemInfo m,
    MonadThread m,
    MonadUnliftIO m
  ) =>
  AnyEvent ref ->
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
    HasLogEnv env,
    MonadIO m,
    MonadLoggerNamespace m,
    MonadQueue m,
    MonadReader env m
  ) =>
  m Void
pollLogQueue = addNamespace "logger" $ do
  queue <- asks getLogQueue
  mfileHandle <- asks (preview (#logFile % _Just % #handle) . getLogEnv)
  let sendFn = maybe BS.putStr toFile mfileHandle
  forever $ do
    logStr <- logStrToBs <$> readQueue queue
    liftIO $ sendFn logStr
  where
    toFile h bs = hPut h bs *> hFlush h
{-# INLINEABLE pollLogQueue #-}
