module Navi.Services.Custom.Multiple.Event
  ( mkMultipleEvent,
  )
where

import DBus.Notify (Note (..))
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Navi.Event.Types
  ( Command (..),
    ErrorNote (..),
    Event (..),
    EventErr (..),
    RepeatEvent (..),
  )
import Navi.Prelude

mkMultipleEvent ::
  Command ->
  [(Text, Note)] ->
  RepeatEvent ref Text ->
  ErrorNote ref ->
  Event ref Text
mkMultipleEvent cmd noteList re en =
  MkEvent
    { eventName = "Multiple",
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
    toEventErr = MkEventErr "Multiple" "Parse error" . T.pack

parseTxt :: [Text] -> Parser Text
parseTxt keys = AP.choice keyParsers
  where
    keyParsers = fmap AP.string keys
