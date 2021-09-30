module Main (main) where

import Control.Concurrent qualified as CC
import Control.Exception (Exception (..), SomeException)
import Control.Exception qualified as Except
import Control.Monad (forever, void)
import DBus.Client (ClientError)
import DBus.Client qualified as DBus
import DBus.Notify (Body (..), Client, Hint (..), Note, Timeout (..), UrgencyLevel (..))
import DBus.Notify qualified as DBusN
import Data.IORef qualified as IORef
import Data.Map.Strict qualified as Map
import Navi.Data.Config (Config (..))
import Navi.Data.Event
  ( AnyEvent (..),
    ErrorEvent (..),
    Event (..),
    RepeatEvent (..),
    Trigger (..),
  )
import Navi.Data.Event qualified as Event
import Navi.Data.NonNegative (NonNegative (MkNonNegative), unsafeNonNegative)
import Navi.Services.Battery qualified as Battery
import Navi.Services.Custom qualified as Custom

-- conf file
-- [general]
-- pollInterval

-- [custom]
-- icon (optional)
-- summary
-- body (optional)
-- urgency (optional)
-- trigger = ''' ... '''

-- [battery]
-- level = natural
-- icon (optional)
-- urgency

-- Steps
-- 1. Parse file into list of triggernotes

-- main loop: when awake, for each triggernote (t, n):
--   if t then send(n) else nothing

config :: IO Config
config = do
  evts <- ioEvts
  pure $
    MkConfig
      { pollInterval = unsafeNonNegative 10,
        events = evts,
        logFile = "navi.log"
      }

ioEvts :: IO [AnyEvent]
ioEvts =
  sequenceA
    [ batteryEvt,
      customEvt
    ]

batteryEvt :: IO AnyEvent
batteryEvt = do
  ref <- IORef.newIORef Nothing
  let a = (10, Battery.batteryNNote 10 Nothing Critical (Milliseconds 10_000))
      ab = (55, Battery.batteryNNote 55 Nothing Critical (Milliseconds 10_000))
      b = (56, Battery.batteryNNote 56 Nothing Critical (Milliseconds 10_000))
      c = (80, Battery.batteryNNote 80 Nothing Normal (Milliseconds 10_000))
      mp = Map.fromList [a, ab, b, c]
      repeatErr = ErrEvt AllowRepeats

  pure $ MkAnyEvent $ Battery.mkBatteryEvent mp (DisallowRepeats ref) repeatErr

customEvt :: IO AnyEvent
customEvt = do
  ref <- IORef.newIORef False
  ref2 <- IORef.newIORef False
  let note = Custom.customNote "Custom" (Just "A note!") Nothing Normal (Milliseconds 10_000)
      repeatErr = ErrEvt $ DisallowRepeats ref2 --ErrEvt AllowRepeats
  pure $ MkAnyEvent $ Custom.mkCustomEvent cmdStr note (DisallowRepeats ref) repeatErr
  where
    cmdStr = "min=`date +%M`; if [[ \"$min % 2\" -eq 0 ]]; then echo true; else echo false; fi"

main :: IO ()
main = do
  eitherClient :: Either ClientError Client <- Except.try DBusN.connectSession
  client <- case eitherClient of
    Left err -> do
      putStrLn $ "Error connecting to dbus: " <> DBus.clientErrorMessage err
      Except.throwIO err
    Right c -> pure c

  MkConfig {events, pollInterval} <- config
  let (MkNonNegative sleepTime) = pollInterval
  forever $ do
    CC.threadDelay (sleepTime * 1_000_000)
    traverse (processEvent client) events

processEvent :: Client -> AnyEvent -> IO ()
processEvent client (MkAnyEvent MkEvent {trigger, getNote, errorEvent}) = do
  result :: Either SomeException a <- Except.try $ runTrigger trigger
  either triggerErr (triggerSuccess getNote) result
  where
    triggerErr :: SomeException -> IO ()
    triggerErr e = do
      putStrLn $ "Exception: " <> displayException e
      case errorEvent of
        NoErrEvt -> pure ()
        ErrEvt AllowRepeats -> void $ DBusN.notify client (exToNote e)
        ErrEvt (DisallowRepeats ref) -> do
          prevErr <- IORef.readIORef ref
          if prevErr
            then pure ()
            else do
              IORef.writeIORef ref True
              void $ DBusN.notify client (exToNote e)

    triggerSuccess :: (a -> Maybe Note) -> a -> IO ()
    triggerSuccess getNoteFn triggerVal =
      maybe (pure ()) sendNote (getNoteFn triggerVal)
      where
        sendNote note = void $ DBusN.notify client note

exToNote :: SomeException -> Note
exToNote ex = Event.mkNote Nothing summary body hints timeout
  where
    summary = "Exception"
    body = Just $ Text $ displayException ex
    hints = [Urgency Critical]
    timeout = Milliseconds 10_000