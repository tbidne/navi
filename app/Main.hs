module Main (main) where

import Data.IORef (IORef)
import Data.Void (absurd)
import Navi (runNavi, runNaviT)
import Navi.Args (getArgs)
import Navi.Prelude

main :: IO ()
main = do
  res <- getArgs >>= runNaviT . runNavi @IORef
  case res of
    Left err -> putStrLn $ showt err
    Right vd -> absurd vd
