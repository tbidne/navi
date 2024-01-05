-- | Provides utilities.
--
-- @since 0.1
module Navi.Utils
  ( -- * TOML

    -- ** Decoding utils
    getFieldOptArrayOf,

    -- ** Specific decoders
    commandDecoder,
    urgencyLevelOptDecoder,

    -- * Misc
    whenJust,
  )
where

import DBus.Notify (UrgencyLevel (Critical, Low, Normal))
import Navi.Prelude
import Pythia.Data.Command (Command (MkCommand))

-- | @since 0.1
whenJust :: (Applicative f) => Maybe a -> (a -> f ()) -> f ()
whenJust m action = maybe (pure ()) action m

-- | Decodes an optional list. This is morally
--
-- @
-- getFieldOptWith (getArrayOf tomlDecoder) :: Text -> Decoder (Maybe [a])
-- @
--
-- except we return an empty list when the key is missing rather than
-- 'Nothing'.
--
-- @since 0.1
getFieldOptArrayOf :: (DecodeTOML a) => Text -> Decoder [a]
getFieldOptArrayOf =
  fmap (fromMaybe [])
    . getFieldOptWith (getArrayOf tomlDecoder)

-- | TOML decoder for optional 'UrgencyLevel' with field name "urgency".
--
-- @since 0.1
urgencyLevelOptDecoder :: Decoder (Maybe UrgencyLevel)
urgencyLevelOptDecoder = getFieldOptWith urgencyLevelDecoder "urgency"

urgencyLevelDecoder :: Decoder UrgencyLevel
urgencyLevelDecoder = do
  t <- tomlDecoder
  case t of
    "low" -> pure Low
    "normal" -> pure Normal
    "critical" -> pure Critical
    bad -> fail $ unpack $ "Invalid value: " <> bad
{-# INLINEABLE urgencyLevelDecoder #-}

-- | TOML decoder for 'Command' with field name "command".
--
-- @since 0.1
commandDecoder :: Decoder Command
commandDecoder = MkCommand <$> getField "command"
