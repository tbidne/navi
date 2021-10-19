module Main (main) where

import Control.Monad.Reader (ReaderT (..))
import Data.Functor.Identity (Identity (..))
import Data.IORef (IORef)
import Data.Text qualified as T
import Data.Void (absurd)
import Navi (runNavi, runNaviT)
import Navi.Args (Args (..), getArgs)
import Navi.Config (Config, readConfig)
import Navi.Effects (MonadMutRef (..))
import Navi.Env (mkEnv)
import Navi.Prelude
import System.Exit qualified as Exit

main :: IO ()
main = do
  eConfig <- getArgs >>= tryParseConfig @IORef
  config <- tryOrDie eConfig

  eEnv <- mkEnv config
  env <- tryOrDie eEnv

  absurd <$> runReaderT (runNaviT runNavi) env
  where
    tryOrDie e = either exit pure e
    exit e = do
      putStrLn e
      Exit.die $ T.unpack e

tryParseConfig :: MonadMutRef IO ref => Args Identity -> IO (Either Text (Config ref))
tryParseConfig =
  fmap (first mkErr)
    . readConfig
    . runIdentity
    . configFile
  where
    mkErr = (<>) "Config error: " . showt
