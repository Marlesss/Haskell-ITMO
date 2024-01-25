{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import HW6.T3
import Prettyprinter.Comonad19
import Options.Parser

import Control.Monad.Trans.Except
import System.Random

main :: IO ()
main = do
  config <- parse
  case runExcept $ validateAppConfig config of
    (Left e)                 -> putDocLn $ prettyOptionsError e
    (Right (AppConfig {..})) -> do
      stdGen <- getStdGen
      let grids = take iterations $ simulate comonad19Config stdGen
      putDocLn $ prettyComonad19Grids gridSize grids
