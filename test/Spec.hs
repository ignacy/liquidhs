{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Applicative
import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Liquid


main :: IO ()
main = hspec $
  describe "Liquid Parser" $ do
      describe "Assignment" $ do
        it "parses simple assignment statement" $
          parse whileParser ""
          "{% assign alfa = \"10\" %}" `shouldParse` (Assign "alfa" "10" [])

        it "parses simple assignment statement with filter" $
          parse whileParser ""
          "{% assign alfa = \"10\" | upcase %}" `shouldParse` (Assign "alfa" "10" [FilterName "upcase"])

        it "parses simple assignment statement with multiple filtes " $
          parse whileParser "" "{% assign alfa = \"10\" | upcase | split: ',' %}" `shouldParse`
          (Assign "alfa" "10" [FilterName "upcase", ParameterizedFilterName "split" "',' "])

      describe "Yaml Preamble" $ do
          it "slurps all yaml configuration items" $
            parse whileParser ""
            "--- slug: 'my_page' name: 'some name' --- "
            `shouldParse`
            (YAMLPreamble "slug: 'my_page' name: 'some name' ")

      describe "More complete example " $ do
          it "parses multiline liquid file with many tags" $
            parse whileParser ""
            "--- slug: 'hello' --- {% assign ala = \"ma kota\" %} {% comment %} You should ignore me {% endcomment %}"
            `shouldParse`
            (Seq [YAMLPreamble "slug: 'hello' ", Assign "ala" "ma kota" []])

      describe "Capture block " $ do
          it "consumes capture and it does parse it " $
            parse whileParser ""
            "{% capture zlapanaGrupa %} {% assign ala = \"mam kota\" %} {% endcapture %}"
            `shouldParse`
            (Capture "zlapanaGrupa" (Assign "ala" "mam kota" []))

      describe "Catches errors " $ do
          it "knows when there is no closing tag" $
            parse whileParser "" `shouldFailOn` "{%"

          it "knows when there is no opening tag" $
            parse whileParser "" `shouldFailOn` "%}"

      describe "HTML fragments " $ do
          it "parses HTML element" $
            parse whileParser ""
            " <html><body></body></html> "
            `shouldParse`
            (HtmlTag "html" (HtmlAttribute "") (HtmlTag "body" (HtmlAttribute "") (StringLiteral "")))
