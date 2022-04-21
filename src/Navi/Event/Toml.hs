-- | This modules provides toml configuration related to events.
module Navi.Event.Toml
  ( RepeatEvtToml (..),
    repeatEvtCodec,
    repeatEvtTomlToVal,
    mRepeatEvtTomlToVal,
    ErrorNoteToml (..),
    errorNoteCodec,
    errorNoteTomlToVal,
    mErrorNoteTomlToVal,
    word16Codec,
  )
where

import Control.Category ((>>>))
import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Event.Types (ErrorNote (..), RepeatEvent (..))
import Navi.Prelude
import Toml
  ( AnyValue,
    BiMap (..),
    Key,
    TomlBiMap,
    TomlBiMapError (..),
    TomlCodec,
  )
import Toml qualified

-- | TOML for 'Event.Event'.
data EventConfig = MkEventConfig
  { repeatEvt :: Maybe RepeatEvtToml,
    errEvt :: Maybe ErrorNoteToml
  }
  deriving stock (Eq, Show)

-- | TOML for 'RepeatEvent'.
data RepeatEvtToml
  = NoRepeatsToml
  | AllowRepeatsToml
  deriving stock (Eq, Show)

-- | Codec for 'RepeatEvtToml'.
repeatEvtCodec :: TomlCodec RepeatEvtToml
repeatEvtCodec = Toml.dimap toBool fromBool $ Toml.bool "repeat-events"
  where
    fromBool True = AllowRepeatsToml
    fromBool False = NoRepeatsToml
    toBool AllowRepeatsToml = True
    toBool NoRepeatsToml = False

-- | Constructs a mutable 'RepeatEvent' from 'RepeatEvtToml'.
repeatEvtTomlToVal :: MonadMutRef ref m => RepeatEvtToml -> m (RepeatEvent ref a)
repeatEvtTomlToVal AllowRepeatsToml = pure AllowRepeats
repeatEvtTomlToVal NoRepeatsToml = NoRepeats <$> newRef Nothing

-- | Constructs a mutable 'RepeatEvent' from 'RepeatEvtToml'. If none is
-- provided, defaults to 'NoRepeatsToml', i.e., no repeats.
mRepeatEvtTomlToVal :: MonadMutRef ref m => Maybe RepeatEvtToml -> m (RepeatEvent ref a)
mRepeatEvtTomlToVal Nothing = repeatEvtTomlToVal NoRepeatsToml
mRepeatEvtTomlToVal (Just t) = repeatEvtTomlToVal t

-- | TOML for 'ErrorNote'.
data ErrorNoteToml
  = NoErrNoteToml
  | ErrNoteAllowRepeatsToml
  | ErrNoteNoRepeatsToml
  deriving stock (Eq, Show)

-- | Codec for 'ErrorNoteToml'.
errorNoteCodec :: TomlCodec ErrorNoteToml
errorNoteCodec = Toml.textBy showErrEvt parseErrEvt "error-events"
  where
    showErrEvt NoErrNoteToml = "none"
    showErrEvt ErrNoteAllowRepeatsToml = "repeats"
    showErrEvt ErrNoteNoRepeatsToml = "no-repeats"
    parseErrEvt "none" = Right NoErrNoteToml
    parseErrEvt "repeats" = Right ErrNoteAllowRepeatsToml
    parseErrEvt "no-repeats" = Right ErrNoteNoRepeatsToml
    parseErrEvt other = Left $ "Unsupported error event key: " <> other

-- | Constructs a mutable 'ErrorNote' from 'ErrorNoteToml'.
errorNoteTomlToVal :: MonadMutRef ref m => ErrorNoteToml -> m (ErrorNote ref)
errorNoteTomlToVal NoErrNoteToml = pure NoErrNote
errorNoteTomlToVal ErrNoteAllowRepeatsToml = pure $ AllowErrNote AllowRepeats
errorNoteTomlToVal ErrNoteNoRepeatsToml = AllowErrNote . NoRepeats <$> newRef Nothing

-- | Constructs a mutable 'ErrorNote' from 'ErrorNoteToml'. If none is
-- provided, defaults to 'ErrNoteNoRepeatsToml', i.e., we /do/ send
-- notifications for errors, but we do not send repeats.
mErrorNoteTomlToVal :: MonadMutRef ref m => Maybe ErrorNoteToml -> m (ErrorNote ref)
mErrorNoteTomlToVal Nothing = errorNoteTomlToVal ErrNoteNoRepeatsToml
mErrorNoteTomlToVal (Just t) = errorNoteTomlToVal t

-- | Parses a TOML 'Word16'.
word16Codec :: Key -> TomlCodec Word16
word16Codec = Toml.match _Word16

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
