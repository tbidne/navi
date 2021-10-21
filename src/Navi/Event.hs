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
    logEvent,
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
import Katip (Severity (..))
import Navi.Effects (MonadLogger (..), MonadMutRef (..), MonadShell (..))
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
  ( MonadLogger m,
    MonadShell m,
    Show a
  ) =>
  Event ref a ->
  m (Either EventErr a)
runEvent evt@MkEvent {eventName, command, parser} = addNamespace "Run Event" $ do
  eResultStr <- execSh command
  case eResultStr of
    Left ex -> do
      let exStr = T.pack $ displayException ex
      logEvent evt ErrorS $ "Exception: " <> exStr
      pure $ Left $ MkEventErr eventName "Exception" exStr
    Right resultStr -> do
      let parsed = parser resultStr
      logEvent evt DebugS $ "Parsed: " <> showt parsed
      pure parsed

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

logEvent :: MonadLogger m => Event ref a -> Severity -> Text -> m ()
logEvent MkEvent {eventName} s t = logText s $ "[" <> eventName <> "] " <> t
