-- | This modules provides toml configuration related to events.
module Navi.Event.Toml
  ( RepeatEventToml (..),
    repeatEventCodec,
    repeatEventTomlToVal,
    mRepeatEventTomlToVal,
    ErrorNoteToml (..),
    errorNoteCodec,
    errorNoteTomlToVal,
    mErrorNoteTomlToVal,
  )
where

import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Event.Types (ErrorNote (..), RepeatEvent (..))
import Navi.Prelude
import Toml (TomlCodec)
import Toml qualified

-- | TOML for 'Event.Event'.
data EventConfig = MkEventConfig
  { repeatEvent :: Maybe RepeatEventToml,
    errEvent :: Maybe ErrorNoteToml
  }
  deriving stock (Eq, Show)

-- | TOML for 'RepeatEvent'.
data RepeatEventToml
  = NoRepeatsToml
  | AllowRepeatsToml
  deriving stock (Eq, Show)

-- | Codec for 'RepeatEventToml'.
repeatEventCodec :: TomlCodec RepeatEventToml
repeatEventCodec = Toml.dimap toBool fromBool $ Toml.bool "repeat-events"
  where
    fromBool True = AllowRepeatsToml
    fromBool False = NoRepeatsToml
    toBool AllowRepeatsToml = True
    toBool NoRepeatsToml = False
{-# INLINEABLE repeatEventCodec #-}

-- | Constructs a mutable 'RepeatEvent' from 'RepeatEventToml'.
repeatEventTomlToVal :: MonadMutRef ref m => RepeatEventToml -> m (RepeatEvent ref a)
repeatEventTomlToVal AllowRepeatsToml = pure AllowRepeats
repeatEventTomlToVal NoRepeatsToml = NoRepeats <$> newRef Nothing
{-# INLINEABLE repeatEventTomlToVal #-}

-- | Constructs a mutable 'RepeatEvent' from 'RepeatEventToml'. If none is
-- provided, defaults to 'NoRepeatsToml', i.e., no repeats.
mRepeatEventTomlToVal :: MonadMutRef ref m => Maybe RepeatEventToml -> m (RepeatEvent ref a)
mRepeatEventTomlToVal Nothing = repeatEventTomlToVal NoRepeatsToml
mRepeatEventTomlToVal (Just t) = repeatEventTomlToVal t
{-# INLINEABLE mRepeatEventTomlToVal #-}

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
{-# INLINEABLE errorNoteCodec #-}

-- | Constructs a mutable 'ErrorNote' from 'ErrorNoteToml'.
errorNoteTomlToVal :: MonadMutRef ref m => ErrorNoteToml -> m (ErrorNote ref)
errorNoteTomlToVal NoErrNoteToml = pure NoErrNote
errorNoteTomlToVal ErrNoteAllowRepeatsToml = pure $ AllowErrNote AllowRepeats
errorNoteTomlToVal ErrNoteNoRepeatsToml = AllowErrNote . NoRepeats <$> newRef Nothing
{-# INLINEABLE errorNoteTomlToVal #-}

-- | Constructs a mutable 'ErrorNote' from 'ErrorNoteToml'. If none is
-- provided, defaults to 'ErrNoteNoRepeatsToml', i.e., we /do/ send
-- notifications for errors, but we do not send repeats.
mErrorNoteTomlToVal :: MonadMutRef ref m => Maybe ErrorNoteToml -> m (ErrorNote ref)
mErrorNoteTomlToVal Nothing = errorNoteTomlToVal ErrNoteNoRepeatsToml
mErrorNoteTomlToVal (Just t) = errorNoteTomlToVal t
{-# INLINEABLE mErrorNoteTomlToVal #-}
