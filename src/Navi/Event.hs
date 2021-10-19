module Navi.Event
  ( -- * Event type
    Event (..),
    AnyEvent (..),
    Command (..),
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
    mkNote,
  )
where

import Control.Exception (Exception (..))
import DBus.Notify
  ( Body (..),
    Hint (..),
    Icon (..),
    Note (..),
    Timeout (..),
  )
import Data.Text qualified as T
import Navi.Effects (MonadMutRef (..), MonadShell (..))
import Navi.Event.Types
  ( AnyEvent (..),
    Command (..),
    ErrorNote (..),
    Event (..),
    EventErr (..),
    RepeatEvent (..),
  )
import Navi.Prelude

runEvent ::
  ( MonadMutRef m ref,
    MonadShell m
  ) =>
  Event ref a ->
  m (Either EventErr a)
runEvent MkEvent {eventName, command, parser} = do
  eResultStr <- execSh command
  pure $ case eResultStr of
    Left ex -> Left $ MkEventErr eventName "Exception" $ T.pack $ displayException ex
    Right resultStr -> parser resultStr

blockRepeat :: (Eq a, MonadMutRef m ref) => RepeatEvent ref a -> a -> m Bool
blockRepeat repeatEvt newVal = do
  case repeatEvt of
    -- Repeat events are allowed, so do not block.
    AllowRepeats -> pure False
    -- Repeat events are not allowed, must check.
    NoRepeats prevRef -> do
      prevVal <- readRef prevRef
      if prevVal == Just newVal
        then -- Already sent this alert, block.
          pure True
        else -- New alert, do not block.
        do
          writeRef prevRef $ Just newVal
          pure False

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

-- | Helper function for creating notifications.
mkNote :: Maybe Icon -> String -> Maybe Body -> [Hint] -> Timeout -> Note
mkNote icon summary body hints timeout =
  Note
    { appName = "navi",
      appImage = icon,
      summary = summary,
      body = body,
      actions = [],
      hints = hints,
      expiry = timeout
    }
