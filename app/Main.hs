{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Liquid
import Compiler (compile)
import Params
import Text.Megaparsec
import Data.Void
import qualified Data.Text as T

default (T.Text)

printParsingResults :: Either (ParseErrorBundle T.Text Data.Void.Void) LiquidObject -> String
printParsingResults eitherLiquidOrError =
    case (eitherLiquidOrError) of
        Left errorBundle -> errorBundlePretty errorBundle
        Right success -> show success

work :: Params -> IO ()
work params = do
  liquidTemplate <- readFile (fname params)
  case (Params.parse params) of
    False -> putStr $ printParsingResults(Text.Megaparsec.parse whileParser "" (T.pack liquidTemplate))
    True -> putStr $ compile(Text.Megaparsec.parse whileParser "" (T.pack liquidTemplate))

main :: IO ()
main = cmdLineParser >>= work
