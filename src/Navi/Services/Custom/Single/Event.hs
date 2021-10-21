module Navi.Services.Custom.Single.Event
  ( mkSingleEvent,
  )
where

import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote)
import Navi.Event.Types
  ( Command (..),
    ErrorNote (..),
    Event (..),
    RepeatEvent (..),
  )
import Navi.Event.Types qualified as ETypes
import Navi.Prelude

mkSingleEvent ::
  Command ->
  (Text, NaviNote) ->
  RepeatEvent ref Bool ->
  ErrorNote ref ->
  Event ref Bool
mkSingleEvent cmd (triggerVal, note) re en =
  MkEvent
    { name = "Single",
      command = cmd,
      parser = parseFn,
      raiseAlert = \b -> if b then Just note else Nothing,
      repeatEvent = re,
      errorNote = en
    }
  where
    parseFn = toServiceErr . AP.parseOnly (parseVal triggerVal)
    toServiceErr =
      first $
        ETypes.MkEventErr "Single" "Parse error"
          . (<>) "Could not parse true/false: "
          . T.pack

parseVal :: Text -> Parser Bool
parseVal triggerVal = AP.skipSpace *> (parseTrigger <|> pure False)
  where
    parseTrigger = AP.string triggerVal $> True <* end
    end = AP.skipSpace *> AP.endOfInput
