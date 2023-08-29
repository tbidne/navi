{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module provides the core application type and logic.
module Navi
  ( -- * Entry point
    runNavi,
  )
where

import DBus.Client (ClientError (clientErrorFatal))
import DBus.Notify (UrgencyLevel (Critical, Normal))
import Data.Text qualified as T
import Effectful.Concurrent.Async qualified as Async
import Effectful.Concurrent.STM.TBQueue.Static (flushTBQueueA)
import Effectful.Concurrent.Static (sleep)
import Effectful.LoggerNS.Dynamic (addNamespace, logStrToBs)
import Effectful.Terminal.Dynamic (putBinary)
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
import Navi.Effectful.Notify (NotifyDynamic, sendNote, sendNoteQueue)
import Navi.Effectful.Pythia (PythiaDynamic)
import Navi.Env.Core
  ( HasEvents (getEvents),
    HasLogEnv (getLogEnv),
    HasNoteQueue (getNoteQueue),
  )
import Navi.Event qualified as Event
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    EventError,
    EventSuccess (MkEventSuccess),
  )
import Navi.Prelude

-- | Entry point for the application.
runNavi ::
  forall env es.
  ( Concurrent :> es,
    HasEvents env,
    HasLogEnv env,
    HasNoteQueue env,
    HandleWriterDynamic :> es,
    IORefStatic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    NotifyDynamic :> es,
    PythiaDynamic :> es,
    TerminalDynamic :> es,
    Reader env :> es
  ) =>
  Eff es Void
runNavi = do
  let welcome =
        MkNaviNote
          { summary = "Navi",
            body = Just "Navi is up :-)",
            urgency = Just Normal,
            timeout = Just $ Seconds 10
          }
  sendNoteQueue @env welcome
  events <- asks @env getEvents
  runAllAsync events
  where
    runAllAsync ::
      ( Traversable t
      ) =>
      t AnyEvent ->
      Eff es Void
    runAllAsync evts =
      Async.withAsync (pollLogQueue @env) $ \logThread -> do
        -- NOTE: Need the link here _before_ we run the other two threads.
        -- This ensures that a logger exception successfully kills the entire
        -- app.
        Async.link logThread
        runEvents evts
          `catchAny` \e -> do
            Async.cancel logThread
            -- handle remaining logs
            queue <- view #logQueue <$> asks @env getLogEnv
            sendFn <- getLoggerFn @env
            flushTBQueueA queue >>= traverse_ (sendFn . logStrToBs)
            throwM e
    {-# INLINEABLE runAllAsync #-}

    -- run events and notify threads
    runEvents :: (Traversable t) => t AnyEvent -> Eff es Void
    runEvents evts =
      Async.withAsync (logExAndRethrow "Notify: " (pollNoteQueue @env)) $ \noteThread ->
        Async.withAsync
          ( logExAndRethrow
              "Event processing: "
              ( Async.mapConcurrently (processEvent @env) evts
              )
          )
          (fmap fst . Async.waitBoth noteThread)
    {-# INLINEABLE runEvents #-}

    logExAndRethrow :: Text -> Eff es a -> Eff es a
    logExAndRethrow prefix io = catchAny io $ \ex -> do
      $(logError) (prefix <> pack (displayException ex))
      throwM ex
    {-# INLINEABLE logExAndRethrow #-}
{-# INLINEABLE runNavi #-}

{- HLINT ignore module "Redundant bracket" -}

processEvent ::
  forall env es.
  ( Concurrent :> es,
    HasNoteQueue env,
    IORefStatic :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PythiaDynamic :> es,
    Reader env :> es
  ) =>
  AnyEvent ->
  Eff es Void
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
    errorNote = event ^. #errorNote

    handleSuccess ::
      (Eq result, Show result) =>
      EventSuccess result ->
      Eff es ()
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
                sendNoteQueue @env note

    {-# INLINEABLE handleSuccess #-}

    handleEventError :: EventError -> Eff es ()
    handleEventError =
      addNamespace "handleEventError"
        . handleErr eventErrToNote
    {-# INLINEABLE handleEventError #-}

    handleSomeException :: SomeException -> Eff es ()
    handleSomeException =
      addNamespace "handleSomeException"
        . handleErr exToNote

    handleErr :: (Exception e) => (e -> NaviNote) -> e -> Eff es ()
    handleErr toNote e = do
      blockErrEvent <- Event.blockErr errorNote
      $(logError) (pack $ displayException e)
      if blockErrEvent
        then $(logDebug) "Error note blocked"
        else sendNoteQueue @env (toNote e)
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
  forall env es.
  ( Concurrent :> es,
    HasNoteQueue env,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    NotifyDynamic :> es,
    Reader env :> es
  ) =>
  Eff es Void
pollNoteQueue = addNamespace "note-poller" $ do
  queue <- asks @env getNoteQueue
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
              <> T.pack (displayException ce)
{-# INLINEABLE pollNoteQueue #-}

pollLogQueue ::
  forall env es.
  ( Concurrent :> es,
    HasLogEnv env,
    LoggerNSDynamic :> es,
    HandleWriterDynamic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  Eff es Void
pollLogQueue = addNamespace "logger" $ do
  queue <- view #logQueue <$> asks @env getLogEnv
  sendFn <- getLoggerFn @env
  forever
    $
    -- NOTE: Rethrow all exceptions
    atomicReadWrite queue (sendFn . logStrToBs)
{-# INLINEABLE pollLogQueue #-}

getLoggerFn ::
  forall env es.
  ( HasLogEnv env,
    HandleWriterDynamic :> es,
    Reader env :> es,
    TerminalDynamic :> es
  ) =>
  Eff es (ByteString -> Eff es ())
getLoggerFn = do
  mfileHandle <- asks @env (view #logHandle . getLogEnv)
  pure $ maybe putBinary toFile mfileHandle
  where
    toFile h bs = hPut h bs *> hFlush h

atomicReadWrite ::
  ( Concurrent :> es
  ) =>
  -- | Queue from which to read.
  TBQueue a ->
  -- | Function to apply.
  (a -> Eff es b) ->
  Eff es ()
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
