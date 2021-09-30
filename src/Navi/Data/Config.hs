module Navi.Data.Config
  ( Config (..),
  )
where

import Navi.Data.Event (AnyEvent)
import Navi.Data.NonNegative (NonNegative)

data Config = MkConfig
  { pollInterval :: NonNegative,
    events :: [AnyEvent],
    logFile :: FilePath
  }
