{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Liquid
import Compiler (compile)
import Params
import Text.Megaparsec
import Data.Void
import Hasql.Session
import Hasql.Statement

import qualified Data.Int

import qualified Data.Text as T
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import qualified Hasql.Connection as Connection

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

-- main :: IO ()
-- main = cmdLineParser >>= work


main :: IO ()
main = do
  Right connection <- Connection.acquire connectionSettings
  result <- Hasql.Session.run (getPage 52) connection
  case result of
    Left qe -> putStr $ "query errpr"
    Right r -> case r of
      Nothing -> putStr $ "Nothing found"
      Just s -> putStr $ printParsingResults(Text.Megaparsec.parse whileParser "" s)
  where
    connectionSettings = Connection.settings "localhost" 5432 "nearme" "nearme" "nearme_development"

getPage :: Data.Int.Int32 -> Session (Maybe T.Text)
getPage id = do
  Hasql.Session.statement id findPageById

findPageById :: Statement Data.Int.Int32 (Maybe T.Text)
findPageById =
  Statement
    "select content from pages where id = $1"
    (E.param E.int4)
    (D.rowMaybe (D.column D.text))
    True
