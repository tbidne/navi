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
  { -- | The name of the event.
    name :: Text,
    -- | Short description of the error.
    short :: Text,
    -- | Long description of the error.
    long :: Text
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''EventError

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
