-- | Provides utilities.
--
-- @since 0.1
module Navi.Utils
  ( -- * TOML

    -- ** Decoding utils
    getFieldOptArrayOf,

    -- ** Specific decoders
    commandDecoder,
    runAppDecoder,
    urgencyLevelOptDecoder,
  )
where

import DBus.Notify (UrgencyLevel (..))
import Navi.Prelude
import Pythia (RunApp (..))
import Pythia.Data.Command (Command (..))

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
getFieldOptArrayOf :: DecodeTOML a => Text -> Decoder [a]
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

-- | TOML decoder for optional 'RunApp' with field name "app". If the field
-- is not found then we decode 'Many'.
--
-- @since 0.1
runAppDecoder :: Decoder a -> Decoder (RunApp a)
runAppDecoder decoder = getFieldOptWith decoder "app" <&> maybe Many Single
{-# INLINEABLE runAppDecoder #-}

-- | TOML decoder for 'Command' with field name "command".
--
-- @since 0.1
commandDecoder :: Decoder Command
commandDecoder = MkCommand <$> getField "command"
