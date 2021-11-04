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
  )
where

import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Event.Types (ErrorNote (..), RepeatEvent (..))
import Navi.Prelude
import Toml (TomlCodec)
import Toml qualified

-- | TOML for 'Event.Event'.
data EventConfig = MkEventConfig
  { repeatEvt :: Maybe RepeatEvtToml,
    errEvt :: Maybe ErrorNoteToml
  }
  deriving (Show)

-- | TOML for 'RepeatEvent'.
data RepeatEvtToml
  = NoRepeatsToml
  | AllowRepeatsToml
  deriving (Show)

-- | Codec for 'RepeatEvtToml'.
repeatEvtCodec :: TomlCodec RepeatEvtToml
repeatEvtCodec = Toml.dimap toBool fromBool $ Toml.bool "repeat-events"
  where
    fromBool True = AllowRepeatsToml
    fromBool False = NoRepeatsToml
    toBool AllowRepeatsToml = True
    toBool NoRepeatsToml = False

-- | Constructs a mutable 'RepeatEvent' from 'RepeatEvtToml'.
repeatEvtTomlToVal :: MonadMutRef m ref => RepeatEvtToml -> m (RepeatEvent ref a)
repeatEvtTomlToVal AllowRepeatsToml = pure AllowRepeats
repeatEvtTomlToVal NoRepeatsToml = NoRepeats <$> newRef Nothing

-- | Constructs a mutable 'RepeatEvent' from 'RepeatEvtToml'. If none is
-- provided, defaults to 'NoRepeatsToml', i.e., no repeats.
mRepeatEvtTomlToVal :: MonadMutRef m ref => Maybe RepeatEvtToml -> m (RepeatEvent ref a)
mRepeatEvtTomlToVal Nothing = repeatEvtTomlToVal NoRepeatsToml
mRepeatEvtTomlToVal (Just t) = repeatEvtTomlToVal t

-- | TOML for 'ErrorNote'.
data ErrorNoteToml
  = NoErrNoteToml
  | ErrNoteAllowRepeatsToml
  | ErrNoteNoRepeatsToml
  deriving (Show)

-- | Codec for 'ErrorNoteToml'.
errorNoteCodec :: TomlCodec ErrorNoteToml
errorNoteCodec = Toml.textBy showErrEvt parseErrEvt "error-events"
  where
    showErrEvt NoErrNoteToml = "none"
    showErrEvt ErrNoteAllowRepeatsToml = "repeats"
    showErrEvt ErrNoteNoRepeatsToml = "no-repeats"
    parseErrEvt "none" = Right NoErrNoteToml
    parseErrEvt "repeats" = Right NoErrNoteToml
    parseErrEvt "no-repeats" = Right NoErrNoteToml
    parseErrEvt other = Left $ "Unsupported error event key: " <> other

-- | Constructs a mutable 'ErrorNote' from 'ErrorNoteToml'.
errorNoteTomlToVal :: MonadMutRef m ref => ErrorNoteToml -> m (ErrorNote ref)
errorNoteTomlToVal NoErrNoteToml = pure NoErrNote
errorNoteTomlToVal ErrNoteAllowRepeatsToml = pure $ AllowErrNote AllowRepeats
errorNoteTomlToVal ErrNoteNoRepeatsToml = AllowErrNote . NoRepeats <$> newRef Nothing

-- | Constructs a mutable 'ErrorNote' from 'ErrorNoteToml'. If none is
-- provided, defaults to 'ErrNoteNoRepeatsToml', i.e., we /do/ send
-- notifications for errors, but we do not send repeats.
mErrorNoteTomlToVal :: MonadMutRef m ref => Maybe ErrorNoteToml -> m (ErrorNote ref)
mErrorNoteTomlToVal Nothing = errorNoteTomlToVal ErrNoteNoRepeatsToml
mErrorNoteTomlToVal (Just t) = errorNoteTomlToVal t
