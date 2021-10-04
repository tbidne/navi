module Navi.Data.Config
  ( Config (..),
  )
where

import Navi.Data.Event (Event)
import Navi.Data.NonNegative (NonNegative)

data Config = MkConfig
  { pollInterval :: NonNegative,
    events :: [Event],
    logFile :: FilePath
  }
