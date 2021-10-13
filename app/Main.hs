module Main (main) where

import Control.Concurrent qualified as CC
import Control.Exception (Exception (..))
import Control.Exception qualified as Except
import DBus.Client (ClientError)
import DBus.Client qualified as DBus
import DBus.Notify (Body (..), Client, Hint (..), Note, Timeout (..), UrgencyLevel (..))
import DBus.Notify qualified as DBusN
import Data.Text qualified as T
import Navi.Config (Config (..))
import Navi.Config qualified as Config
import Navi.Data.NonNegative (NonNegative (..))
import Navi.Event
  ( Event (..),
    EventErr (..),
    EventResult (..),
  )
import Navi.Event qualified as Event
import Navi.Prelude
import System.Directory (XdgDirectory (XdgConfig))
import System.Directory qualified as Dir
import UnexceptionalIO (SomeNonPseudoException)
import UnexceptionalIO qualified as Unexceptional

main :: IO ()
main = do
  configPath <- Dir.getXdgDirectory XdgConfig "navi/config.toml"
  eConfig <- Config.readConfig configPath
  MkConfig {events, pollInterval} <- case eConfig of
    Left errs -> do
      putStrLn "Error reading config"
      Except.throwIO errs
    Right cfg -> pure cfg

  eitherClient :: Either ClientError Client <- Except.try DBusN.connectSession
  client <- case eitherClient of
    Left err -> do
      putStrLn $ "Error connecting to dbus: " <> T.pack (DBus.clientErrorMessage err)
      Except.throwIO err
    Right c -> pure c

  let (MkNonNegative sleepTime) = pollInterval
  forever $ do
    CC.threadDelay (sleepTime * 1_000_000)
    traverse (processEvent client) events

processEvent :: Client -> Event IO -> IO ()
processEvent client MkEvent {trigger, errorEvent} = do
  result <- Unexceptional.fromIO trigger
  either triggerErr triggerSuccess result
  where
    triggerErr :: SomeNonPseudoException -> IO ()
    triggerErr ex = do
      putStrLn $ "Exception: " <> T.pack (displayException ex)
      blockErrEvent <- Event.blockErr errorEvent
      if blockErrEvent
        then pure ()
        else void $ DBusN.notify client (exToNote ex)

    triggerSuccess :: EventResult -> IO ()
    triggerSuccess None = pure ()
    triggerSuccess (Err err) = do
      blockErrEvent <- Event.blockErr errorEvent
      putStrLn $ "Event Error: " <> showt err
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

serviceErrToNote :: EventErr -> Note
serviceErrToNote (MkEventErr nm short _) = Event.mkNote Nothing summary body hints timeout
  where
    summary = "Event Error"
    body = Just $ Text $ T.unpack $ nm <> ": " <> short
    hints = [Urgency Critical]
    timeout = Milliseconds 10_000
