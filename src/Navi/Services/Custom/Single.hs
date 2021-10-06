module Navi.Services.Custom.Single
  ( mkSingleEvent,
    mkSingleNote,
  )
where

import DBus.Notify
  ( Body (..),
    Hint (..),
    Icon (..),
    Note (..),
    Timeout (..),
    UrgencyLevel (..),
  )
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Text qualified as T
import Navi.Data.Event
  ( Command (..),
    ErrorEvent (..),
    Event (..),
    RepeatEvent (..),
  )
import Navi.Data.Event qualified as Event
import Navi.Prelude
import Navi.Services.Types (ServiceErr (..))

mkSingleEvent :: Command -> (Text, Note) -> RepeatEvent Bool -> ErrorEvent -> Event
mkSingleEvent cmd (triggerVal, note) = Event.mkEvent cmd parseFn () lookupFn
  where
    parseFn = toServiceErr . AP.parseOnly (parseVal triggerVal)
    lookupFn _ b = if b then Just note else Nothing
    toServiceErr =
      first $
        MkServiceErr "Single" "Parse error"
          . (<>) "Could not parse true/false: "
          . T.pack

mkSingleNote :: String -> Maybe String -> Maybe Icon -> UrgencyLevel -> Timeout -> Note
mkSingleNote summary body icon urgency = Event.mkNote icon summary body' hints
  where
    body' = fmap Text body
    hints = [Urgency urgency]

parseVal :: Text -> Parser Bool
parseVal triggerVal = AP.skipSpace *> (parseTrigger <|> pure False)
  where
    parseTrigger = AP.string triggerVal $> True <* end
    end = AP.skipSpace *> AP.endOfInput