{-# LANGUAGE TemplateHaskell #-}

-- | This module provides functionality for handling events.
module Navi.Event
  ( -- * Event type
    Event (..),
    AnyEvent (..),
    runEvent,

    -- * Results
    EventSuccess (..),
    EventError (..),

    -- * Caching previous events/errors
    RepeatEvent (..),
    ErrorNote (..),
    blockRepeat,
    blockErr,
    updatePrevTrigger,
  )
where

import Effectful.LoggerNS.Dynamic (addNamespace)
import Navi.Effectful.Pythia (PythiaDynamic, query)
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    ErrorNote (AllowErrNote, NoErrNote),
    Event
      ( MkEvent,
        errorNote,
        name,
        pollInterval,
        raiseAlert,
        repeatEvent,
        serviceType
      ),
    EventError (MkEventError, name, short),
    EventSuccess (MkEventSuccess, raiseAlert, repeatEvent, result),
    RepeatEvent (AllowRepeats, NoRepeats),
  )
import Navi.Prelude

-- | Runs an event, i.e.,
--
-- 1. Queries the system via 'MonadSystemInfo'.
-- 2. Returns the parsed result.
runEvent ::
  ( LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    PythiaDynamic :> es,
    Show result
  ) =>
  Event result ->
  Eff es (EventSuccess result)
runEvent event = addNamespace "runEvent" $ do
  result <- query $ event ^. #serviceType
  $(logInfo) ("Shell returned: " <> showt result)
  pure
    $ MkEventSuccess
      { result,
        repeatEvent = event ^. #repeatEvent,
        raiseAlert = event ^. #raiseAlert
      }
{-# INLINEABLE runEvent #-}

-- | Determines if we should block the event. The semantics are:
--
-- 1. 'AllowRepeats': never block (returns 'False').
-- 2. 'NoRepeats': block only if the parameter @a@ equals the previous @a@
--    stored in our @ref@.
blockRepeat ::
  ( Eq a,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    IORefStatic :> es,
    Show a
  ) =>
  RepeatEvent a ->
  a ->
  Eff es Bool
blockRepeat repeatEvent newVal = addNamespace "blockRepeat" $ do
  case repeatEvent of
    -- Repeat events are allowed, so do not block.
    AllowRepeats -> pure False
    -- Repeat events are not allowed, must check.
    NoRepeats prevRef -> do
      prevVal <- readIORef prevRef
      $(logDebug) ("Previous value: " <> showt prevVal)
      $(logDebug) ("New value: " <> showt newVal)
      if prevVal == Just newVal
        then -- Already sent this alert, block.
          pure True
        else -- New alert, do not block.
        do
          writeIORef prevRef $ Just newVal
          pure False
{-# INLINEABLE blockRepeat #-}

-- | Determines if we should block the error event. The semantics are:
--
-- 1. 'NoErrNote': always block (returns 'True').
-- 2. 'AllowErrNote' 'AllowRepeats': never block (returns 'False').
-- 3. 'AllowErrNote' 'NoRepeats': block only if we have sent a notifcation
--    for this error before.
blockErr ::
  ( LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    IORefStatic :> es
  ) =>
  ErrorNote ->
  Eff es Bool
blockErr errorEvent = addNamespace "blockErr" $ do
  case errorEvent of
    -- Error events are off, block.
    NoErrNote -> do
      $(logDebug) "Error notes are off"
      pure True
    -- Error events are on and repeats allowed, do not block.
    AllowErrNote AllowRepeats -> do
      $(logDebug) "Error notes are on and repeats allowed"
      pure False
    -- Error events are on but repeats not allowed, must check.
    AllowErrNote (NoRepeats ref) -> do
      prevErr <- readIORef ref
      case prevErr of
        -- Already sent this error, block
        Just () -> do
          $(logDebug) "Already sent error"
          pure True
        -- Error not send, do not block
        Nothing -> do
          $(logDebug) "Send error"
          writeIORef ref $ Just ()
          pure False
{-# INLINEABLE blockErr #-}

-- | If the reference is 'NoRepeats' then we overwrite the previous reference
-- with the new parameter. Otherwise we do nothing.
updatePrevTrigger ::
  ( Eq a,
    IORefStatic :> es
  ) =>
  RepeatEvent a ->
  a ->
  Eff es ()
updatePrevTrigger repeatEvent newVal =
  -- Only overwrite value if it's new
  case repeatEvent of
    NoRepeats ref -> do
      val <- readIORef ref
      when (val /= Just newVal) $ writeIORef ref $ Just newVal
    _ -> pure ()
{-# INLINEABLE updatePrevTrigger #-}
