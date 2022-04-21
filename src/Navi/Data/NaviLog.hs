{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NaviLog' type.
module Navi.Data.NaviLog
  ( NaviLog (..),
  )
where

import Katip (Severity)
import Navi.Prelude

-- | The log type used in Navi.
data NaviLog = MkNaviLog
  { severity :: !Severity,
    text :: !Text
  }
  deriving (Eq, Show)

makeFieldLabelsNoPrefix ''NaviLog
