{-# LANGUAGE CPP #-}

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
    displayInner,
    whenJust,
  )
where

#if MIN_VERSION_base(4, 20, 0) && !MIN_VERSION_base(4, 21, 0)
import Control.Exception qualified as E
#endif
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

-- NOTE: For base 4.20 (GHC 9.10), there is a callstack on the SomeException
-- itself. We don't really want this as it clutters the output (and fails
-- a functional test). So in this case we walk the SomeException to avoid
-- the callstack.
--
-- In later base versions, the callstack is separate, so we have no problems.
displayInner :: (Exception e) => e -> String
#if MIN_VERSION_base(4, 20, 0) && !MIN_VERSION_base(4, 21, 0)
displayInner ex = case E.toException ex of
  E.SomeException e -> displayException $ e
#else
displayInner = displayException
#endif
