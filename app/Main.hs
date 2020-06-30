{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Liquid
import Compiler (compile)
import Params
import DB (getPage)
import Text.Megaparsec
import Data.Void
import Hasql.Session


import qualified Data.Text as T
import qualified Hasql.Connection as Connection

default (T.Text)

printParsingResults :: Either (ParseErrorBundle T.Text Data.Void.Void) LiquidObject -> String
printParsingResults = extract
  where
    extract (Left errorBundle) = errorBundlePretty errorBundle
    extract (Right success) = show success

-- work :: Params -> IO ()
-- work params = do
--   liquidTemplate <- readFile (fname params)
--   putStr $ actionResult liquidTemplate $ Params.parse params
--   where
--     actionResult text False = printParsingResults(parseChars text)
--     actionResult text True = compile(parseChars text)

main :: IO ()
main = do
  Right connection <- Connection.acquire connectionSettings
  result <- Hasql.Session.run (getPage 52) connection
  case result of
    Left qe -> putStr $ show qe
    Right r -> case r of
      Nothing -> putStr "Nothing found"
      Just s -> putStr $ printParsingResults(Text.Megaparsec.parse whileParser "" s)
  where
    connectionSettings = Connection.settings "localhost" 5432 "nearme" "nearme" "nearme_development"
