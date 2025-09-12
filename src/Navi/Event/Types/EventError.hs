{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides types for defining notification events.
module Navi.Event.Types.EventError
  ( EventError (..),
  )
where

import Navi.Prelude

-- | Represents an error when querying an 'Event'.
data EventError = MkEventError
  { -- | Long description of the error.
    long :: Text,
    -- | The name of the event.
    name :: Text,
    -- | Short description of the error.
    short :: Text
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "long" k EventError EventError a b
  where
  labelOptic =
    lensVL
      $ \f (MkEventError a1 a2 a3) ->
        fmap
          (\b -> MkEventError b a2 a3)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "name" k EventError EventError a b
  where
  labelOptic =
    lensVL
      $ \f (MkEventError a1 a2 a3) ->
        fmap
          (\b -> MkEventError a1 b a3)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "short" k EventError EventError a b
  where
  labelOptic =
    lensVL
      $ \f (MkEventError a1 a2 a3) ->
        fmap
          (\b -> MkEventError a1 a2 b)
          (f a3)
  {-# INLINE labelOptic #-}

instance Exception EventError where
  displayException e =
    unpackText
      $ mconcat
        [ e ^. #name,
          ": ",
          e ^. #short,
          ". ",
          e ^. #long
        ]
