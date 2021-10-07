module Navi.Event
  ( -- * Event type
    Event (..),
    Command (..),
    mkEvent,

    -- * Results
    EventResult (..),
    EventErr (..),

    -- * Caching previous events/errors
    RepeatEvent (..),
    ErrorNote (..),
    blockRepeat,
    blockErr,
    updatePrevTrigger,

    -- * Helper functions
    mkNote,
    execStr,
  )
where

import DBus.Notify
  ( Body (..),
    Hint (..),
    Icon (..),
    Note (..),
    Timeout (..),
  )
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Navi.Event.Types
  ( Command (..),
    ErrorNote (..),
    Event (..),
    EventErr (..),
    EventResult (..),
    RepeatEvent (..),
  )
import Navi.Prelude
import System.Process qualified as P

mkEvent ::
  Eq a =>
  Command ->
  (Text -> Either EventErr a) ->
  b ->
  (b -> a -> Maybe Note) ->
  RepeatEvent a ->
  ErrorNote ->
  Event
mkEvent cmd parserFn triggerNoteMap lookupFn repeatEvt = MkEvent triggerFn
  where
    triggerFn = mTrigger cmd parserFn triggerNoteMap lookupFn repeatEvt

mTrigger ::
  Eq a =>
  Command ->
  (Text -> Either EventErr a) ->
  b ->
  (b -> a -> Maybe Note) ->
  RepeatEvent a ->
  IO EventResult
mTrigger (MkCommand cmdString) queryParser alertMap lookupFn repeatEvt = do
  resultStr <- execStr cmdString
  case queryParser resultStr of
    Left err -> pure $ Err err
    Right result -> case lookupFn alertMap result of
      Nothing -> updatePrevTrigger repeatEvt result $> None
      Just note -> do
        blocked <- blockRepeat repeatEvt result
        if blocked
          then pure None
          else do
            updatePrevTrigger repeatEvt result
            pure $ Alert note

blockRepeat :: Eq a => RepeatEvent a -> a -> IO Bool
blockRepeat repeatEvt newVal = do
  case repeatEvt of
    -- Repeat events are allowed, so do not block.
    AllowRepeats -> pure False
    -- Repeat events are not allowed, must check.
    NoRepeats prevRef -> do
      prevVal <- IORef.readIORef prevRef
      if prevVal == Just newVal
        then -- Already sent this alert, block.
          pure True
        else -- New alert, do not block.
        do
          IORef.writeIORef prevRef $ Just newVal
          pure False

blockErr :: ErrorNote -> IO Bool
blockErr errorEvent =
  case errorEvent of
    -- Error events are off, block.
    NoErrNote -> pure True
    -- Error events are on and repeats allowed, do not block.
    AllowErrNote AllowRepeats -> pure False
    -- Error events are on but repeats not allowed, must check.
    AllowErrNote (NoRepeats ref) -> do
      prevErr <- IORef.readIORef ref
      case prevErr of
        -- Already sent this error, block
        Just () -> pure True
        -- Error not send, do not block
        Nothing -> do
          IORef.writeIORef ref $ Just ()
          pure False

updatePrevTrigger :: Eq a => RepeatEvent a -> a -> IO ()
updatePrevTrigger repeatEvt newVal =
  -- Only overwrite value if it's new
  case repeatEvt of
    NoRepeats ref -> do
      val <- IORef.readIORef ref
      if val /= Just newVal
        then IORef.writeIORef ref $ Just newVal
        else pure ()
    _ -> pure ()

-- | Executes a shell command.
execStr :: Text -> IO Text
execStr cmd = T.pack <$> P.readCreateProcess process ""
  where
    process = P.shell $ T.unpack cmd

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
