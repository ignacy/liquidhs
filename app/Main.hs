module Main where

import Liquid
import Text.Megaparsec

main :: IO ()
main = do
  input <- getContents
  parseTest whileParser input
