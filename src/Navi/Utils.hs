-- | Provides utilities
module Navi.Utils
  ( word16Codec,
  )
where

import Control.Category ((>>>))
import Navi.Prelude
import Toml
  ( AnyValue,
    BiMap (..),
    TomlBiMap,
    TomlBiMapError (..),
    TomlCodec,
  )
import Toml qualified

-- | Parses a TOML 'Word16'.
word16Codec :: TomlCodec Word16
word16Codec = Toml.match _Word16 "poll-interval"

_Word16 :: TomlBiMap Word16 AnyValue
_Word16 = _Word16Int >>> Toml._Int

_Word16Int :: TomlBiMap Word16 Int
_Word16Int = BiMap (Right . fromIntegral) parseW16
  where
    parseW16 i
      | i < 0 = Left $ ArbitraryError $ "Received negative for word16: " <> showt i
      | i > w16ToInt (maxBound :: Word16) =
          Left $ ArbitraryError $ "Too large for word16: " <> showt i
      | otherwise = Right $ intToWord16 i
    intToWord16 :: Int -> Word16
    intToWord16 = fromIntegral
