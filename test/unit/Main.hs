-- | Entrypoint for unit tests.
--
-- @since 0.1
module Main (main) where

import Data.Function (($))
import Test.Tasty qualified as Tasty
import Unit.Navi.Config qualified as Config
import Unit.Navi.Config.Toml qualified as Config.Toml
import Unit.Navi.Services.Battery.Percentage.Toml qualified as S.Battery.Percentage.Toml
import Unit.Navi.Services.Battery.Status.Toml qualified as S.Battery.Status.Toml
import Unit.Navi.Services.Custom.Toml qualified as S.Custom.Toml
import Unit.Navi.Services.Network.NetInterfaces.Toml qualified as S.Network.NetInterfaces.Toml
import Unit.Prelude hiding (($))

-- This is annoying. We want to enable the useful -Wunused-packages warning.
-- Because we are using a custom "fat" prelude, we actually do not require
-- base as a dependency for this test unit. Unfortunately, not having an
-- explicit dependency on base causes ghcid to fail. Thus we have several
-- options:
--
-- 1. Disable -Wunused-packages on this test suite.
-- 2. Give up on ghcid.
-- 3. "Require" base
--
-- Because both the warning and ghcid are useful, we choose to go with 3,
-- hence the hiding/import of ($) above.

-- | Runs unit tests.
main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "Unit tests"
      [ Config.tests,
        Config.Toml.tests,
        S.Battery.Percentage.Toml.tests,
        S.Battery.Status.Toml.tests,
        S.Custom.Toml.tests,
        S.Network.NetInterfaces.Toml.tests
      ]
