module Main (main) where

import Control.Concurrent qualified as CC
import Control.Exception (Exception (..))
import Control.Exception qualified as Except
import Control.Monad (forever, void)
import DBus.Client (ClientError)
import DBus.Client qualified as DBus
import DBus.Notify (Body (..), Client, Hint (..), Note, Timeout (..), UrgencyLevel (..))
import DBus.Notify qualified as DBusN
import Data.IORef qualified as IORef
import Data.Text qualified as T
import Navi.Data.Config (Config (..))
import Navi.Data.Event
  ( Command (..),
    ErrorEvent (..),
    Event (..),
    RepeatEvent (..),
  )
import Navi.Data.Event qualified as Event
import Navi.Data.NonNegative (NonNegative (MkNonNegative), unsafeNonNegative)
import Navi.Services.Battery qualified as Battery
import Navi.Services.Custom.Single qualified as Single
import Navi.Services.Types (ServiceErr (..), ServiceResult (..))
import UnexceptionalIO (SomeNonPseudoException)
import UnexceptionalIO qualified as Unexceptional

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

ioEvts :: IO [Event]
ioEvts =
  sequenceA
    [ batteryEvt,
      singleEvt
    ]

batteryEvt :: IO Event
batteryEvt = do
  ref <- IORef.newIORef Nothing
  let a = (10, Battery.batteryNNote 10 Nothing Critical (Milliseconds 10_000))
      ab = (55, Battery.batteryNNote 55 Nothing Critical (Milliseconds 10_000))
      b = (97, Battery.batteryNNote 56 Nothing Critical (Milliseconds 10_000))
      c = (98, Battery.batteryNNote 80 Nothing Normal (Milliseconds 10_000))
      mp = [a, ab, b, c]
      repeatErr = ErrEvt AllowRepeats

  pure $ Battery.mkBatteryEvent mp (DisallowRepeats ref) repeatErr

singleEvt :: IO Event
singleEvt = do
  ref <- IORef.newIORef Nothing
  ref2 <- IORef.newIORef Nothing
  let note = Single.mkSingleNote "Single" (Just "A note!") Nothing Normal (Milliseconds 10_000)
      repeatErr = ErrEvt $ DisallowRepeats ref2
  pure $ Single.mkSingleEvent cmd ("true", note) (DisallowRepeats ref) repeatErr
  where
    cmd = MkCommand "min=`date +%M`; if [[ \"$min % 2\" -eq 0 ]]; then echo true; else echo false; fi"

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

processEvent :: Client -> Event -> IO ()
processEvent client MkEvent {trigger, errorEvent} = do
  result <- Unexceptional.fromIO trigger
  either triggerErr triggerSuccess result
  where
    triggerErr :: SomeNonPseudoException -> IO ()
    triggerErr ex = do
      putStrLn $ "Exception: " <> displayException ex
      blockErrEvent <- Event.blockErr errorEvent
      if blockErrEvent
        then pure ()
        else void $ DBusN.notify client (exToNote ex)

    triggerSuccess :: ServiceResult -> IO ()
    triggerSuccess None = pure ()
    triggerSuccess (Err err) = do
      blockErrEvent <- Event.blockErr errorEvent
      putStrLn $ "Service Error: "
      if blockErrEvent
        then pure ()
        else sendNote (serviceErrToNote err)
    triggerSuccess (Alert nt) = sendNote nt

    sendNote note = void $ DBusN.notify client note

exToNote :: SomeNonPseudoException -> Note
exToNote ex = Event.mkNote Nothing summary body hints timeout
  where
    summary = "Exception"
    body = Just $ Text $ displayException ex
    hints = [Urgency Critical]
    timeout = Milliseconds 10_000

serviceErrToNote :: ServiceErr -> Note
serviceErrToNote (MkServiceErr nm short _) = Event.mkNote Nothing summary body hints timeout
  where
    summary = "Service Error"
    body = Just $ Text $ T.unpack $ nm <> ": " <> short
    hints = [Urgency Critical]
    timeout = Milliseconds 10_000