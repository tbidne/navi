{-# LANGUAGE ImplicitPrelude #-}

module Main (main) where

import Control.Exception (Exception (displayException))
import GHC.Conc (setUncaughtExceptionHandler)
import Navi.Runner qualified as Runner

main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)
  Runner.makeEnvAndRun
