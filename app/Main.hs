{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Compiler         (compile)
import           Data.Void
import           DB               (getPage)
import           Hasql.Session
import           Liquid
import           Params
import           Text.Megaparsec


import qualified Data.Text        as T
import qualified Hasql.Connection as Connection

default (T.Text)

printParsingResults :: Either (ParseErrorBundle T.Text Data.Void.Void) LiquidObject -> String
printParsingResults = extract
  where
    extract (Left errorBundle) = errorBundlePretty errorBundle
    extract (Right success)    = show success

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
  --  result <- Hasql.Session.run (getPage 52) connection
  mapM_ (processPage connection) [10..40]
  where
    processPage :: Connection.Connection -> Int -> IO ()
    processPage connection pageId = do
      result <- Hasql.Session.run (getPage (fromIntegral pageId)) connection
      case result of
        Left qe -> putStr $ show qe
        Right r -> case r of
          Nothing -> putStrLn $ "Page with ID=" ++ show pageId ++ " not found"
          Just s -> putStrLn $ "---> For Page with ID=" ++ show pageId ++ printParsingResults(Text.Megaparsec.parse whileParser "" s)
    connectionSettings = Connection.settings "localhost" 5432 "nearme" "nearme" "nearme_development"
