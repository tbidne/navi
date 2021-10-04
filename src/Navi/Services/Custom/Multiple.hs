module Navi.Services.Custom.Multiple
  ( mkMultipleEvent,
  )
where

import DBus.Notify (Note (..))
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Bifunctor qualified as Bifunctor
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Navi.Data.Event (Command (..), ErrorEvent (..), Event (..), RepeatEvent (..))
import Navi.Data.Event qualified as Event
import Navi.Services.Types (ServiceErr (..))

mkMultipleEvent :: Command -> [(Text, Note)] -> RepeatEvent Text -> ErrorEvent -> Event
mkMultipleEvent cmd noteList = Event.mkEvent cmd parser noteMap lookupFn
  where
    noteMap = Map.fromList noteList
    parser = parseFn $ fmap fst noteList
    lookupFn = flip Map.lookup

parseFn :: [Text] -> Text -> Either ServiceErr Text
parseFn keys = Bifunctor.first toServiceErr . AP.parseOnly (parseTxt keys)
  where
    toServiceErr = MkServiceErr "Multiple" "Parse error" . T.pack

parseTxt :: [Text] -> Parser Text
parseTxt keys = AP.choice keyParsers
  where
    keyParsers = fmap AP.string keys