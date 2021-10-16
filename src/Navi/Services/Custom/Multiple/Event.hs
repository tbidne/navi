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
import Navi.Effects (MonadMutRef, MonadShell)
import Navi.Event qualified as Event
import Navi.Event.Types
  ( Command (..),
    ErrorNote (..),
    Event (..),
    EventErr (..),
    RepeatEvent (..),
  )
import Navi.Prelude

mkMultipleEvent ::
  (MonadMutRef m ref, MonadShell m) =>
  Command ->
  [(Text, Note)] ->
  RepeatEvent ref Text ->
  ErrorNote ref ->
  Event m ref
mkMultipleEvent cmd noteList = Event.mkEvent "Multiple" cmd parser noteMap lookupFn
  where
    noteMap = Map.fromList noteList
    parser = parseFn $ fmap fst noteList
    lookupFn = flip Map.lookup

parseFn :: [Text] -> Text -> Either EventErr Text
parseFn keys = first toEventErr . AP.parseOnly (parseTxt keys)
  where
    toEventErr = MkEventErr "Multiple" "Parse error" . T.pack

parseTxt :: [Text] -> Parser Text
parseTxt keys = AP.choice keyParsers
  where
    keyParsers = fmap AP.string keys
