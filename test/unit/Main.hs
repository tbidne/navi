-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Test.Tasty qualified as Tasty
import Unit.Navi.Config qualified as Config
import Unit.Navi.Config.Toml qualified as Config.Toml
import Unit.Navi.Services.Battery.Percentage.Toml qualified as S.Battery.Percentage.Toml
import Unit.Navi.Services.Battery.Status.Toml qualified as S.Battery.Status.Toml
import Unit.Navi.Services.Network.NetInterfaces.Toml qualified as S.Network.NetInterfaces.Toml
import Unit.Prelude

-- | Runs unit tests.
main :: IO ()
main = do
  Tasty.defaultMain $
    Tasty.testGroup
      "Unit tests"
      [ Config.tests,
        Config.Toml.tests,
        S.Battery.Percentage.Toml.tests,
        S.Battery.Status.Toml.tests,
        S.Network.NetInterfaces.Toml.tests
      ]
