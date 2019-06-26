module Main where

import Liquid
import Text.Megaparsec
import Data.Void


printParsingResults :: Either (ParseErrorBundle String Data.Void.Void) LiquidObject -> String
printParsingResults eitherLiquidOrError =
    case (eitherLiquidOrError) of
        Left errorBundle -> errorBundlePretty errorBundle
        Right success -> show success

main :: IO ()
main = do
  input <- getContents
  --  parseTest whileParser input
  putStr $ printParsingResults(parse whileParser "" input)

