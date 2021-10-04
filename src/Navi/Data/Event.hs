module Navi.Data.Event
  ( -- * Event type
    Event (..),
    Command (..),
    mkEvent,

    -- * Caching previous events/errors
    RepeatEvent (..),
    ErrorEvent (..),
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
import Data.Functor (($>))
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.Text (Text)
import Data.Text qualified as T
import Navi.Services.Types (ServiceErr (..), ServiceResult (..))
import System.Process qualified as P

newtype Command = MkCommand {getCommand :: Text}
  deriving (Show)

-- | Determines if we are allowed to send off duplicate notifications
-- simultaneously. If we are not, then 'DisallowRepeats' holds the last trigger
-- so that we can detect duplicates.
data RepeatEvent a
  = DisallowRepeats (IORef (Maybe a))
  | AllowRepeats

data ErrorEvent
  = NoErrEvt
  | ErrEvt (RepeatEvent ())

-- | 'Event' represents sending notifications.
data Event = MkEvent
  { trigger :: IO ServiceResult,
    errorEvent :: ErrorEvent
  }

mkEvent ::
  Eq a =>
  Command ->
  (Text -> Either ServiceErr a) ->
  b ->
  (b -> a -> Maybe Note) ->
  RepeatEvent a ->
  ErrorEvent ->
  Event
mkEvent cmd parserFn triggerNoteMap lookupFn repeatEvt = MkEvent triggerFn
  where
    triggerFn = mTrigger cmd parserFn triggerNoteMap lookupFn repeatEvt

mTrigger ::
  Eq a =>
  Command ->
  (Text -> Either ServiceErr a) ->
  b ->
  (b -> a -> Maybe Note) ->
  RepeatEvent a ->
  IO ServiceResult
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
    DisallowRepeats prevRef -> do
      prevVal <- IORef.readIORef prevRef
      if prevVal == Just newVal
        then -- Already sent this alert, block.
          pure True
        else -- New alert, do not block.
        do
          IORef.writeIORef prevRef $ Just newVal
          pure False

blockErr :: ErrorEvent -> IO Bool
blockErr errorEvent =
  case errorEvent of
    -- Error events are off, block.
    NoErrEvt -> pure True
    -- Error events are on and repeats allowed, do not block.
    ErrEvt AllowRepeats -> pure False
    -- Error events are on but repeats not allowed, must check.
    ErrEvt (DisallowRepeats ref) -> do
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
    DisallowRepeats ref -> do
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