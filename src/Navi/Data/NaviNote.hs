{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'Note' decoders.
module Navi.Data.NaviNote
  ( noteDecoder,
    urgencyLevelOptDecoder,
    timeoutOptDecoder,
    replaceOut,
  )
where

import Data.Bits (toIntegralSized)
import Data.Text qualified as T
import Effects.Notify qualified as Notify
import Navi.Prelude
import Navi.Utils (urgencyLevelOptDecoder)

-- | TOML decoder for optional 'NotifyTimeout' with field name "timeout".
--
-- @since 0.1
timeoutOptDecoder :: Decoder (Maybe NotifyTimeout)
timeoutOptDecoder = getFieldOptWith d "timeout"
  where
    d = makeDecoder $ \case
      String "never" -> pure NotifyTimeoutNever
      String bad -> invalidValue strErr (String bad)
      Integer i -> case toIntegralSized i of
        Just i' -> pure $ NotifyTimeoutMillis $ i' * 1_000
        Nothing -> invalidValue tooLargeErr (Integer i)
      badTy -> typeMismatch badTy
      where
        tooLargeErr = "NotifyTimeout integer too large. Max is: " <> showt maxW16
        strErr = "Unexpected timeout. Only valid string is 'never'."
        maxW16 = maxBound @Word16

-- | @since 0.1
noteDecoder :: Decoder Note
noteDecoder = do
  body <- getFieldOpt "body"
  summary <- getField "summary"
  timeout <- timeoutOptDecoder
  urgency <- urgencyLevelOptDecoder
  pure
    . Notify.setBody body
    . Notify.setTimeout timeout
    . Notify.setUrgency urgency
    . Notify.mkNote
    $ summary

replaceOut :: Text -> Note -> Note
replaceOut outVal = over' (#body % _Just) (T.replace "$out" outVal)
