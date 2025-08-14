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

import DBus.Client (ClientError (clientErrorFatal))
import DBus.Notify (UrgencyLevel (Critical, Normal))
import Effects.Concurrent.Async qualified as Async
import Effects.Concurrent.STM (flushTBQueueA)
import Effects.Concurrent.Thread (MonadThread (labelThread, myThreadId), sleep)
import Effects.Logger.Namespace (logStrToBs)
import Effects.System.Terminal (MonadTerminal (putBinary))
import Navi.Data.NaviLog (LogEnv)
import Navi.Data.NaviNote
  ( NaviNote
      ( MkNaviNote,
        body,
        summary,
        timeout,
        urgency
      ),
    Timeout (Seconds),
  )
import Navi.Effects.MonadNotify (MonadNotify (sendNote))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo)
import Navi.Env.Core
  ( HasEvents (getEvents),
    HasLogEnv (getLogEnv),
    HasNoteQueue (getNoteQueue),
    sendNoteQueue,
  )
import Navi.Event qualified as Event
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    EventError,
    EventSuccess (MkEventSuccess),
  )
import Navi.NaviT (NaviT (..), runNaviT)
import Navi.Prelude
import Navi.Utils qualified as U

-- | Entry point for the application.
runNavi ::
  forall env m k.
  ( HasCallStack,
    HasEvents env,
    HasLogEnv env,
    HasNoteQueue env,
    MonadAsync m,
    MonadHandleWriter m,
    MonadIORef m,
    MonadLoggerNS m env k,
    MonadMask m,
    MonadNotify m,
    MonadSTM m,
    MonadSystemInfo m,
    MonadTerminal m,
    MonadThread m
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
  runAllAsync events
  where
    runAllAsync ::
      ( HasCallStack,
        Traversable t
      ) =>
      t AnyEvent ->
      m Void
    runAllAsync evts = do
      mLogEnv <- asks getLogEnv
      case mLogEnv of
        -- 1. No logging: just run the events.
        Nothing -> runEvents evts
        -- 2. Logging: Run the events and the logger.
        Just logEnv -> do
          Async.withAsync (pollLogQueue logEnv) $ \logThread -> do
            -- NOTE: Need the link here _before_ we run the other two threads.
            -- This ensures that a logger exception successfully kills the entire
            -- app.
            Async.link logThread
            runEvents evts
              `catchSync` \e -> do
                Async.cancel logThread
                -- handle remaining logs
                let queue = logEnv ^. #logQueue
                    sendFn = getLoggerFn logEnv
                flushTBQueueA queue >>= traverse_ (sendFn . logStrToBs)
                throwM e

    -- run events and notify threads
    runEvents :: (HasCallStack, Traversable t) => t AnyEvent -> m Void
    runEvents evts =
      Async.withAsync (logExAndRethrow "Notify: " pollNoteQueue) $ \noteThread ->
        Async.withAsync
          ( logExAndRethrow
              "Event processing: "
              (Async.mapConcurrently processEvent evts)
          )
          (fmap fst . Async.waitBoth noteThread)

    logExAndRethrow :: Text -> m a -> m a
    logExAndRethrow prefix io = catchSync io $ \ex -> do
      $(logError) (prefix <> displayExceptiont ex)
      throwM ex
{-# INLINEABLE runNavi #-}

{- HLINT ignore module "Redundant bracket" -}

processEvent ::
  forall m env k.
  ( HasCallStack,
    HasNoteQueue env,
    MonadCatch m,
    MonadIORef m,
    MonadLoggerNS m env k,
    MonadSTM m,
    MonadSystemInfo m,
    MonadThread m
  ) =>
  AnyEvent ->
  m Void
processEvent (MkAnyEvent event) = addNamespace name $ do
  tid <- myThreadId
  labelThread tid (unpackText name)
  let pi = event ^. (#pollInterval % #unPollInterval)
  forever $ do
    $(logInfo) ("Checking " <> name)
    (Event.runEvent event >>= handleSuccess)
      `catch` handleEventError
      `catchSync` handleSomeException
    sleep pi
  where
    name = event ^. #name
    errorNote = event ^. #errorNote

    handleSuccess ::
      (HasCallStack, Ord trigger, Show result, Show trigger) =>
      EventSuccess result trigger ->
      m ()
    handleSuccess (MkEventSuccess result repeatEvent raiseAlert) =
      addNamespace "handleSuccess" $ do
        case raiseAlert result of
          Nothing -> do
            $(logDebug) ("No alert to raise " <> showt result)
            Event.updatePrevTrigger repeatEvent Nothing
          Just (trigger, note) -> do
            blocked <- Event.blockRepeat repeatEvent trigger
            if blocked
              then $(logDebug) ("Alert blocked " <> showt result)
              else do
                $(logInfo) ("Sending note " <> showt note)
                Event.updatePrevTrigger repeatEvent (Just trigger)
                sendNoteQueue note

    handleEventError :: (HasCallStack) => EventError -> m ()
    handleEventError =
      addNamespace "handleEventError"
        . handleErr eventErrToNote

    handleSomeException :: (HasCallStack) => SomeException -> m ()
    handleSomeException =
      addNamespace "handleSomeException"
        . handleErr exToNote

    handleErr :: (HasCallStack, Exception e) => (e -> NaviNote) -> e -> m ()
    handleErr toNote e = do
      blockErrEvent <- Event.blockErr errorNote
      $(logError) (displayExceptiont e)
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

exToNote :: SomeException -> NaviNote
exToNote ex =
  MkNaviNote
    { summary = "Exception",
      body = Just $ packText (U.displayInner ex),
      urgency = Just Critical,
      timeout = Nothing
    }

pollNoteQueue ::
  ( HasCallStack,
    HasNoteQueue env,
    MonadCatch m,
    MonadLoggerNS m env k,
    MonadNotify m,
    MonadSTM m
  ) =>
  m Void
pollNoteQueue = addNamespace "note-poller" $ do
  queue <- asks getNoteQueue
  forever
    $ readTBQueueA queue
    >>= \nn ->
      sendNote nn `catch` \ce ->
        -- NOTE: Rethrow all exceptions except:
        --
        -- 1. Non-fatal dbus errors e.g. quickly sending the same notif twice.
        if clientErrorFatal ce
          then throwM ce
          else
            $(logError)
              $ "Received non-fatal dbus error: "
              <> packText (displayException ce)
{-# INLINEABLE pollNoteQueue #-}

pollLogQueue ::
  ( HasCallStack,
    MonadLoggerNS m env k,
    MonadHandleWriter m,
    MonadMask m,
    MonadSTM m,
    MonadTerminal m
  ) =>
  LogEnv ->
  m Void
pollLogQueue logEnv = addNamespace "logger" $ do
  forever
    $
    -- NOTE: Rethrow all exceptions
    atomicReadWrite queue (sendFn . logStrToBs)
  where
    queue = logEnv ^. #logQueue
    sendFn = getLoggerFn logEnv
{-# INLINEABLE pollLogQueue #-}

getLoggerFn ::
  ( HasCallStack,
    MonadHandleWriter m,
    MonadTerminal m
  ) =>
  LogEnv ->
  (ByteString -> m ())
getLoggerFn logEnv = maybe putBinary toFile mfileHandle
  where
    mfileHandle = logEnv ^. #logHandle
    toFile h bs = hPut h bs *> hFlush h
{-# INLINEABLE getLoggerFn #-}

atomicReadWrite ::
  ( HasCallStack,
    MonadMask m,
    MonadSTM m
  ) =>
  -- | Queue from which to read.
  TBQueue a ->
  -- | Function to apply.
  (a -> m b) ->
  m ()
atomicReadWrite queue logAction =
  -- NOTE: There are several options we could take here:
  --
  -- 1. uninterruptibleMask_ $ tryReadTBQueueA queue >>= traverse_ logAction
  --
  --    This gives us guaranteed atomicity, at the risk of a possible deadlock,
  --    if either the read or logAction blocks indefinitely. IMPORTANT: If we
  --    go this route, readTBQueueA _must_ be swapped for tryReadTBQueueA, as
  --    the former relies on cancellation via an async exception i.e.
  --    uninterruptibleMask_ + readTBQueueA = deadlock.
  --
  -- 2. mask $ \restore -> restore (readTBQueueA queue) >>= void . logAction
  --
  --    This does not give us absolute atomicity, as logAction could be
  --    interrupted if it is actually blocking; but that is probably the right
  --    choice (responsiveness), and we have atomicity as long as logAction
  --    does not block.
  --
  -- 3. mask_ $ readTBQueueA queue >>= void . logAction
  --
  --    Slightly simpler than 2, has the same caveat regarding atomicity.
  --    The difference is that in the latter, readTBQueueA is also masked
  --    as long as it is not blocking. There really is no reason for this,
  --    as the invariant we care about is _if_ successful read then
  --    successful handle.
  mask $ \restore -> restore (readTBQueueA queue) >>= void . logAction
{-# INLINEABLE atomicReadWrite #-}
