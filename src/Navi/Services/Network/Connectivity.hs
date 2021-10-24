-- | This module provides a service for network connectivity.
module Navi.Services.Network.Connectivity
  ( toNetworkConnectivityEvent,
  )
where

import Control.Applicative qualified as A
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects (MonadMutRef)
import Navi.Event (AnyEvent (..))
import Navi.Event qualified as Event
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( Command (..),
    Event (..),
    EventErr,
  )
import Navi.Prelude
import Navi.Services.Network.Connectivity.Toml (NetworkConnectivityToml)
import Navi.Services.Network.Types (ConnState (..), ConnType (..), Connection (MkConnection), NetworkCommand (..))
import Optics.Core ((%))
import Optics.Fold qualified as O
import Optics.Getter qualified as O
import Optics.Operators ((^.))

-- | Transforms toml configuration data into an 'AnyEvent'.
toNetworkConnectivityEvent ::
  (MonadMutRef m ref) =>
  NetworkConnectivityToml ->
  m (AnyEvent ref)
toNetworkConnectivityEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  pure $
    MkAnyEvent $
      MkEvent
        { name = "Network Connectivity",
          command = cmd,
          parser = tryParseConnection (toml ^. #deviceName),
          raiseAlert = toNote toml,
          repeatEvent = repeatEvt,
          errorNote = errorNote
        }
  where
    cmd = networkToCmd $ toml ^. #networkCommand

tryParseConnection :: Text -> Text -> Either EventErr Connection
tryParseConnection deviceName txt = case AP.parseOnly (A.many parseConnection) txt of
  Right conns ->
    case findDevice conns of
      Just conn -> Right conn
      Nothing ->
        let devices = O.foldlOf' (O.folded % #device) combineDevices "" conns
         in Left $ mkServiceErr $ connErr devices
  Left err -> Left $ mkServiceErr $ T.pack err
  where
    findDevice = headMaybe . filter ((== deviceName) . O.view #device)
    combineDevices t "" = t
    combineDevices t acc = t <> ", " <> acc

    connErr e =
      "No connections matching device `"
        <> deviceName
        <> "`. Devices found: "
        <> e
    mkServiceErr = Event.MkEventErr "Network Connectivity" "Parse error"

toNote :: NetworkConnectivityToml -> Connection -> Maybe NaviNote
toNote noteToml conn =
  Just $
    MkNaviNote
      { summary = "Network Connectivity",
        body = Just body,
        urgency = Nothing,
        timeout = noteToml ^. #mTimeout,
        image = noteToml ^. #mImage
      }
  where
    deviceTxt = showt $ conn ^. #device
    nameTxt = fromMaybe "Unknown" $ conn ^. #name
    body = "Device " <> deviceTxt <> stateTxt
    stateTxt = case conn ^. #state of
      Connected -> " is connected to: " <> nameTxt
      Disconnected -> " is disconnected from: " <> nameTxt
      Unavailable -> " is unavailable"
      Unmanaged -> " is unmanaged"
      UnknownState txt -> " is in an unknown state: " <> txt

parseConnection :: Parser Connection
parseConnection =
  MkConnection
    <$> parseDevice
    <*> parseType
    <*> parseState
    <*> parseName

parseDevice :: Parser Text
parseDevice =
  AP.string "DEVICE:"
    *> AP.skipSpace
    *> AP.takeWhile1 (not . AP.isEndOfLine)
    <* AP.endOfLine

parseType :: Parser ConnType
parseType =
  AP.string "TYPE:"
    *> AP.skipSpace
    *> parseAll
  where
    parseAll =
      AP.choice
        [ AP.string "wifi" <* AP.endOfLine $> Wifi,
          AP.string "wifi-p2p" <* AP.endOfLine $> Wifi_P2P,
          AP.string "ethernet" <* AP.endOfLine $> Ethernet,
          AP.string "loopback" <* AP.endOfLine $> Loopback,
          AP.string "tun" <* AP.endOfLine $> Tun,
          UnknownType <$> AP.takeWhile1 (not . AP.isEndOfLine) <* AP.endOfLine
        ]

parseState :: Parser ConnState
parseState =
  AP.string "STATE:"
    *> AP.skipSpace
    *> parseAll
  where
    parseAll =
      AP.choice
        [ AP.string "connected" <* AP.endOfLine $> Connected,
          AP.string "disconnected" <* AP.endOfLine $> Disconnected,
          AP.string "unavailable" <* AP.endOfLine $> Unavailable,
          AP.string "unmanaged" <* AP.endOfLine $> Unmanaged,
          UnknownState <$> AP.takeWhile1 (not . AP.isEndOfLine) <* AP.endOfLine
        ]

parseName :: Parser (Maybe Text)
parseName =
  AP.string "CONNECTION:"
    *> AP.skipSpace
    *> (parseHyphens <|> parseCName)
    <* AP.endOfLine
  where
    parseHyphens = AP.string "--" $> Nothing
    parseCName = Just <$> AP.takeWhile1 (not . AP.isEndOfLine)

networkToCmd :: NetworkCommand -> Command
networkToCmd NetworkManager = MkCommand "nmcli -m multiline device | cat"
networkToCmd (Custom c) = MkCommand c
