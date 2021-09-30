module Navi.Data.Event
  ( Trigger (..),
    RepeatEvent (..),
    Event (..),
    ErrorEvent (..),
    mkNote,
    execStr,
    AnyEvent (..),
  )
where

import DBus.Notify
  ( Body (..),
    Hint (..),
    Icon (..),
    Note (..),
    Timeout (..),
  )
import Data.IORef (IORef)
import System.Process qualified as P

-- | Event trigger, produces an @a@ that is fed into 'getNote' that determines
-- if we send a 'Note'. For simple events this can just be Bool, and then
-- we might have
--
-- @
-- sendEvent' note True = Just evt
-- sendEvent' note False = Nothing
-- @
--
-- Notice this implies the 'sendEvent' in our 'Event' is a partially applied
-- @sendEvent'@. For a more complicated case, considering sending off
-- different notifications, e.g., different messages/urgencies for different
-- battery levels.
--
-- @
-- trigger :: IO (Maybe Int)
-- trigger = do
--   currBattery <- getCurrBattery
--   if
--      | currBattery < 30 -> Just 30
--      | currBattery < 10 -> Just 10
--      | otherwise -> Nothing
--
-- sendNote normalNote _ _ = Nothing
-- sendNote normalNote criticalNote (Just alertLvl) =
--   if
--      | alertLvl == 30 -> Just normalNote
--      | alertLvl == 10 -> Just criticalNote
--      | otherwise -> error "Bad alert level"
-- @
--
-- Just realized the @fmap sendNote runTrigger :: IO (Maybe Note)@ is not
-- necessarily total, as there's no guarantee @sendNote :: a -> Note@ is
-- total. E.g., @runTrigger :: IO (Maybe Int), sendNote :: Maybe Int -> Maybe Note@
-- is total only because we're using a map that falls back to Maybe. But for
-- battery, we really should have an underlying @getNote :: Int -> Note@ or
-- some such. But that's obviously not total.
--
-- Q: Can I somehow enforce these line up at the type level? Probably not...
-- Maybe not in general, but I might be able to use dependent-map in
-- Battery to enforce that it's present
newtype Trigger a = MkTrigger {runTrigger :: IO a}

-- | Determines if we are allowed to send off duplicate notifications
-- simultaneously. If we are not, then 'DisallowRepeats' holds the last trigger
-- so that we can detect duplicates.
data RepeatEvent a
  = DisallowRepeats (IORef a)
  | AllowRepeats

data ErrorEvent
  = NoErrEvt
  | ErrEvt (RepeatEvent Bool)

-- | 'Event' represents sending notifications based on some 'Trigger'.
-- Its 'repeatEvt' is used to handle sending duplicate notifications.
data Event a = MkEvent
  { trigger :: Trigger a,
    getNote :: a -> Maybe Note,
    repeatEvt :: RepeatEvent a,
    errorEvent :: ErrorEvent
  }

-- | Executes a shell command.
execStr :: String -> IO String
execStr cmd = P.readCreateProcess process ""
  where
    process = P.shell cmd

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

-- | Wraps an 'Event', existentially quantifying its 'Trigger' type, so that
-- we can hold different types in the same list.
data AnyEvent where
  MkAnyEvent :: Event a -> AnyEvent