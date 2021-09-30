module Navi.Services.Custom
  ( mkCustomEvent,
    customNote,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Control.Exception qualified as Except
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
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Navi.Class.Service (Service (..))
import Navi.Data.Event (ErrorEvent (..), Event (..), RepeatEvent (..), Trigger (..))
import Navi.Data.Event qualified as Event
import Navi.Data.ServiceException qualified as ServiceEx
import Navi.Utils qualified as NSUtils

mkCustomEvent :: String -> Note -> RepeatEvent Bool -> ErrorEvent -> Event Bool
mkCustomEvent cmdStr note repeatEvt = MkEvent trigger getNote repeatEvt
  where
    trigger = MkTrigger $ customTrigger cmdStr repeatEvt
    getNote b = if b then Just note else Nothing

newtype CustomEx = MkCustomEx String
  deriving (Show)

instance Service CustomEx where
  name = "Custom"

instance Exception CustomEx where
  displayException (MkCustomEx err) = err
  toException = ServiceEx.toServiceEx
  fromException = ServiceEx.fromServiceEx

customTrigger :: String -> RepeatEvent Bool -> IO Bool
customTrigger cmdStr repeatEvt = do
  res <- T.pack <$> Event.execStr cmdStr
  let res' = parseCustom res
  case res' of
    Left _ ->
      throwErr $
        "Could not parse custom return val as true/false: `" <> T.unpack res <> "`"
    Right False -> NSUtils.updatePrevTrigger repeatEvt False $> False
    Right True -> NSUtils.triggerIfNotRepeat repeatEvt False True
  where
    throwErr str = Except.throwIO $ MkCustomEx str

customNote :: String -> Maybe String -> Maybe Icon -> UrgencyLevel -> Timeout -> Note
customNote summary body icon urgency = Event.mkNote icon summary body' hints
  where
    body' = fmap Text body
    hints = [Urgency urgency]

parseCustom :: Text -> Either String Bool
parseCustom = AP.parseOnly parseBool

parseBool :: Parser Bool
parseBool = AP.skipSpace *> (true <|> false)
  where
    true = AP.string "true" $> True <* end
    false = AP.string "false" $> False <* end
    end = AP.skipSpace *> AP.endOfInput