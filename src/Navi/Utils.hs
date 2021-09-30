module Navi.Utils
  ( triggerIfNotRepeat,
    updatePrevTrigger,
  )
where

import Data.IORef qualified as IORef
import Navi.Data.Event (RepeatEvent (..))

-- | Refactor to monoid maybe?
triggerIfNotRepeat :: Eq a => RepeatEvent a -> a -> a -> IO a
triggerIfNotRepeat repeatEvt emptyVal newVal = do
  case repeatEvt of
    -- Repeat events are allowed so send off the (possibly duplicate) alert.
    AllowRepeats -> pure newVal
    -- Repeat events are not allowed, must check.
    DisallowRepeats prevRef -> do
      prevVal <- IORef.readIORef prevRef
      if prevVal == newVal
        then -- Already sent this alert, do nothing.
          pure emptyVal
        else -- New alert, update cached trigger and send.
        do
          IORef.writeIORef prevRef newVal
          pure newVal

updatePrevTrigger :: Eq a => RepeatEvent a -> a -> IO ()
updatePrevTrigger repeatEvt newVal =
  -- Only overwrite value if it's new
  case repeatEvt of
    DisallowRepeats ref -> do
      val <- IORef.readIORef ref
      if val /= newVal
        then IORef.writeIORef ref newVal
        else pure ()
    _ -> pure ()