{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Liquid
import Text.Megaparsec
import Data.Void
import qualified Data.Text as T

default (T.Text)

printParsingResults :: Either (ParseErrorBundle T.Text Data.Void.Void) LiquidObject -> String
printParsingResults eitherLiquidOrError =
    case (eitherLiquidOrError) of
        Left errorBundle -> errorBundlePretty errorBundle
        Right success -> show success

main :: IO ()
main = do
  input <- getContents
  --  parseTest whileParser input
  putStr $ printParsingResults(parse whileParser "" (T.pack input))
