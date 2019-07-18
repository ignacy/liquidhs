{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Core
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

-- main = do
--   input <- getContents
--   --  parseTest whileParser input
--   putStr $ printParsingResults(parse whileParser "" (T.pack input))

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
