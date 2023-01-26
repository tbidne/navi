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
import DBus.Notify (UrgencyLevel (..))
import Data.Text qualified as T
import Effects.Concurrent.Async qualified as Async
import Effects.Concurrent.STM (flushTBQueueM)
import Effects.Concurrent.Thread (sleep)
import Effects.LoggerNamespace
  ( MonadLoggerNamespace,
    addNamespace,
    logStrToBs,
  )
import Effects.System.Terminal (MonadTerminal (putBinary))
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
import Navi.Event.Types (AnyEvent (..), EventError (..), EventSuccess (..))
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
    MonadHandleWriter m,
    MonadIORef m,
    MonadLoggerNamespace m,
    MonadMask m,
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
  runAllAsync events
  where
    runAllAsync ::
      ( HasCallStack,
        Traversable t
      ) =>
      t AnyEvent ->
      m Void
    runAllAsync evts =
      Async.withAsync pollLogQueue $ \logThread -> do
        -- NOTE: Need the link here _before_ we run the other two threads.
        -- This ensures that a logger exception successfully kills the entire
        -- app.
        Async.link logThread
        runEvents evts
          `catchAny` \e -> do
            Async.cancel logThread
            -- handle remaining logs
            queue <- asks getLogQueue
            sendFn <- getLoggerFn
            flushTBQueueM queue >>= traverse_ (sendFn . logStrToBs)
            throwM e
    {-# INLINEABLE runAllAsync #-}

    -- run events and notify threads
    runEvents :: (HasCallStack, Traversable t) => t AnyEvent -> m Void
    runEvents evts =
      Async.withAsync (logExAndRethrow "Notify: " pollNoteQueue) $ \noteThread ->
        Async.withAsync
          ( logExAndRethrow
              "Event processing: "
              ( Async.mapConcurrently processEvent evts
              )
          )
          (fmap fst . Async.waitBoth noteThread)
    {-# INLINEABLE runEvents #-}

    logExAndRethrow :: Text -> m a -> m a
    logExAndRethrow prefix io = catchAny io $ \ex -> do
      $(logError) (prefix <> pack (displayException ex))
      throwM ex
    {-# INLINEABLE logExAndRethrow #-}
{-# INLINEABLE runNavi #-}

processEvent ::
  forall m env.
  ( HasCallStack,
    HasNoteQueue env,
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
      `catchWithCS` handleEventError
      `catchAny` handleSomeException
    sleep pi
  where
    name = event ^. #name
    errorNote = event ^. #errorNote

    handleSuccess ::
      (HasCallStack, Eq result, Show result) =>
      EventSuccess result ->
      m ()
    handleSuccess (MkEventSuccess result repeatEvent raiseAlert) =
      addNamespace "handleSuccess" $ do
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

    {-# INLINEABLE handleSuccess #-}

    handleEventError :: HasCallStack => EventError -> m ()
    handleEventError =
      addNamespace "handleEventError"
        . handleErr eventErrToNote
    {-# INLINEABLE handleEventError #-}

    handleSomeException :: HasCallStack => SomeException -> m ()
    handleSomeException =
      addNamespace "handleSomeException"
        . handleErr exToNote

    handleErr :: (HasCallStack, Exception e) => (e -> NaviNote) -> e -> m ()
    handleErr toNote e = do
      blockErrEvent <- Event.blockErr errorNote
      $(logError) (pack $ displayException e)
      if blockErrEvent
        then $(logDebug) "Error note blocked"
        else sendNoteQueue (toNote e)
    {-# INLINEABLE handleErr #-}
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
  ( HasCallStack,
    HasNoteQueue env,
    MonadCatch m,
    MonadLoggerNamespace m,
    MonadNotify m,
    MonadReader env m,
    MonadSTM m
  ) =>
  m Void
pollNoteQueue = addNamespace "note-poller" $ do
  queue <- asks getNoteQueue
  forever $
    readTBQueueM queue >>= \nn ->
      sendNote nn `catchWithCS` \ce ->
        -- NOTE: Rethrow all exceptions except:
        --
        -- 1. Non-fatal dbus errors e.g. quickly sending the same notif twice.
        if clientErrorFatal ce
          then throwM ce
          else
            $(logError) $
              "Received non-fatal dbus error: " <> T.pack (displayException ce)
{-# INLINEABLE pollNoteQueue #-}

pollLogQueue ::
  ( HasCallStack,
    HasLogQueue env,
    HasLogEnv env,
    MonadLoggerNamespace m,
    MonadHandleWriter m,
    MonadMask m,
    MonadReader env m,
    MonadSTM m,
    MonadTerminal m
  ) =>
  m Void
pollLogQueue = addNamespace "logger" $ do
  queue <- asks getLogQueue
  sendFn <- getLoggerFn
  forever $
    -- NOTE: Rethrow all exceptions
    atomicReadWrite queue (sendFn . logStrToBs)
{-# INLINEABLE pollLogQueue #-}

getLoggerFn ::
  ( HasCallStack,
    HasLogEnv env,
    MonadHandleWriter m,
    MonadReader env m,
    MonadTerminal m
  ) =>
  m (ByteString -> m ())
getLoggerFn = do
  mfileHandle <- asks (preview (#logFile % _Just % #handle) . getLogEnv)
  pure $ maybe putBinary toFile mfileHandle
  where
    toFile h bs = hPut h bs *> hFlush h

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
  -- 1. uninterruptibleMask_ $ tryReadTBQueueM queue >>= traverse_ logAction
  --
  --    This gives us guaranteed atomicity, at the risk of a possible deadlock,
  --    if either the read or logAction blocks indefinitely. IMPORTANT: If we
  --    go this route, readTBQueueM _must_ be swapped for tryReadTBQueueM, as
  --    the former relies on cancellation via an async exception i.e.
  --    uninterruptibleMask_ + readTBQueueM = deadlock.
  --
  -- 2. mask $ \restore -> restore (readTBQueueM queue) >>= void . logAction
  --
  --    This does not give us absolute atomicity, as logAction could be
  --    interrupted if it is actually blocking; but that is probably the right
  --    choice (responsiveness), and we have atomicity as long as logAction
  --    does not block.
  --
  -- 3. mask_ $ readTBQueueM queue >>= void . logAction
  --
  --    Slightly simpler than 2, has the same caveat regarding atomicity.
  --    The difference is that in the latter, readTBQueueM is also masked
  --    as long as it is not blocking. There really is no reason for this,
  --    as the invariant we care about is _if_ successful read then
  --    successful handle.
  mask $ \restore -> restore (readTBQueueM queue) >>= void . logAction
