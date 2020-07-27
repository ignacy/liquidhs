{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}


module Compiler where

import           Data.Text       (intercalate)
import qualified Data.Text       as T
import           Data.Void
import           Liquid
import           Text.Megaparsec

default (T.Text)

asString :: LiquidObject -> T.Text
asString (LIdentifier t) = t
asString (Assign name value filters) = name <> " => " <> value
asString (Capture text object) = text <> " => " <> asString object
asString (HtmlTag name (HtmlAttribute attributes) object) = "<" <> name <> " " <> attributes <> ">" <> asString object <> "</" <> name <> ">"
asString (StringLiteral s) = s
asString (YAMLPreamble t) = t
asString (Seq objects) = intercalate "\n" (fmap asString objects)
asString other = T.pack (show other)

compileLiquid :: LiquidObject -> T.Text
compileLiquid = asString

compile :: Either (ParseErrorBundle T.Text Data.Void.Void) LiquidObject -> String
compile eitherLiquidOrError =
  case eitherLiquidOrError of
    Left errorBundle -> errorBundlePretty errorBundle
    Right success    -> T.unpack (compileLiquid success)
