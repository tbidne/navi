-- | This module provides the core logic for constructing an event with
-- multiple alert conditions.
module Navi.Services.Custom.Multiple.Event
  ( mkMultipleEvent,
  )
where

import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote)
import Navi.Event.Types
  ( Command (..),
    ErrorNote (..),
    Event (..),
    EventErr,
    RepeatEvent (..),
  )
import Navi.Event.Types qualified as ETypes
import Navi.Prelude

-- | Constructs an 'Event' for multiple alerts.
mkMultipleEvent ::
  Command ->
  [(Text, NaviNote)] ->
  RepeatEvent ref Text ->
  ErrorNote ref ->
  Event ref Text
mkMultipleEvent cmd noteList re en =
  MkEvent
    { name = "Multiple",
      command = cmd,
      parser = parseFn $ fmap fst noteList,
      raiseAlert = flip Map.lookup noteMap,
      repeatEvent = re,
      errorNote = en
    }
  where
    noteMap = Map.fromList noteList

parseFn :: [Text] -> Text -> Either EventErr Text
parseFn keys = first toEventErr . AP.parseOnly (parseTxt keys)
  where
    toEventErr = ETypes.MkEventErr "Multiple" "Parse error" . T.pack

parseTxt :: [Text] -> Parser Text
parseTxt keys = AP.choice keyParsers
  where
    keyParsers = fmap AP.string keys
