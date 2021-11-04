-- | This module provides functionality for handling events.
module Navi.Event
  ( -- * Event type
    Event (..),
    AnyEvent (..),
    runEvent,

    -- * Results
    EventErr (..),

    -- * Caching previous events/errors
    RepeatEvent (..),
    ErrorNote (..),
    blockRepeat,
    blockErr,
    updatePrevTrigger,

    -- * Helper functions
    logEvent,
  )
where

import Katip (Severity (..))
import Navi.Effects (MonadLogger (..), MonadMutRef (..), MonadShell (..))
import Navi.Event.Types
  ( AnyEvent (..),
    ErrorNote (..),
    Event (..),
    EventErr (..),
    RepeatEvent (..),
  )
import Navi.Event.Types qualified as ETypes
import Navi.Prelude
import Optics.Operators ((^.))

-- | Runs an event, i.e.,
--
-- 1. Queries the system via 'execSh'.
-- 2. Returns the parsed result.
runEvent ::
  ( MonadLogger m,
    MonadShell m,
    Show result
  ) =>
  Event ref result ->
  m (Either EventErr result)
runEvent event = addNamespace "Run Event" $ do
  eResult <- execSh $ event ^. #serviceType
  logEvent event DebugS $ "Shell returned: " <> showt eResult
  pure $ first ETypes.fromQueryError eResult

-- | Determines if we should block the event. The semantics are:
--
-- 1. 'AllowRepeats': never block (returns 'False').
-- 2. 'NoRepeats': block only if the parameter @a@ equals the previous @a@
--    stored in our @ref@.
blockRepeat :: (Eq a, MonadLogger m, MonadMutRef m ref, Show a) => RepeatEvent ref a -> a -> m Bool
blockRepeat repeatEvt newVal = addNamespace "Checking event repeats" $ do
  case repeatEvt of
    -- Repeat events are allowed, so do not block.
    AllowRepeats -> pure False
    -- Repeat events are not allowed, must check.
    NoRepeats prevRef -> do
      prevVal <- readRef prevRef
      logText DebugS $ "Previous value: " <> showt prevVal
      logText DebugS $ "New value: " <> showt newVal
      if prevVal == Just newVal
        then -- Already sent this alert, block.
          pure True
        else -- New alert, do not block.
        do
          writeRef prevRef $ Just newVal
          pure False

-- | Determines if we should block the error event. The semantics are:
--
-- 1. 'NoErrNote': always block (returns 'True').
-- 2. 'AllowErrNote' 'AllowRepeats': never block (returns 'False').
-- 3. 'AllowErrNote' 'NoRepeats': block only if we have sent a notifcation
--    for this error before.
blockErr :: MonadMutRef m ref => ErrorNote ref -> m Bool
blockErr errorEvent =
  case errorEvent of
    -- Error events are off, block.
    NoErrNote -> pure True
    -- Error events are on and repeats allowed, do not block.
    AllowErrNote AllowRepeats -> pure False
    -- Error events are on but repeats not allowed, must check.
    AllowErrNote (NoRepeats ref) -> do
      prevErr <- readRef ref
      case prevErr of
        -- Already sent this error, block
        Just () -> pure True
        -- Error not send, do not block
        Nothing -> do
          writeRef ref $ Just ()
          pure False

-- | If the reference is 'NoRepeats' then we overwrite the previous reference
-- with the new parameter. Otherwise we do nothing.
updatePrevTrigger :: (Eq a, MonadMutRef m ref) => RepeatEvent ref a -> a -> m ()
updatePrevTrigger repeatEvt newVal =
  -- Only overwrite value if it's new
  case repeatEvt of
    NoRepeats ref -> do
      val <- readRef ref
      if val /= Just newVal
        then writeRef ref $ Just newVal
        else pure ()
    _ -> pure ()

-- | Helper function for logging events.
logEvent :: MonadLogger m => Event ref a -> Severity -> Text -> m ()
logEvent event s t = logText s $ "[" <> event ^. #name <> "] " <> t
