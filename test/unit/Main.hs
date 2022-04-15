-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Test.Tasty qualified as Tasty
import Unit.Navi.Config qualified as Navi.Config
import Unit.Navi.Config.Toml qualified as Navi.Config.Toml
import Unit.Prelude

-- | Runs unit tests.
--
-- @since 0.1
main :: IO ()
main = do
  Tasty.defaultMain $
    Tasty.testGroup
      "Unit tests"
      [ Navi.Config.tests,
        Navi.Config.Toml.tests
      ]
