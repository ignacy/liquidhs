{-# LANGUAGE OverloadedStrings #-}

module Liquid where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

data LiquidObject
  = LIdentifier String
  | Assign String LiquidObject
  | Capture String LiquidObject
  | StringLiteral String
  | YAMLPreamble String
  | Seq [LiquidObject]
  deriving (Show)

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipBlockComment "{% comment %}" "{% endcomment %}"
    blockCmnt = L.skipBlockComment "{% comment %}" "{% endcomment %}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "{{") (symbol "}}")

curlys :: Parser a -> Parser a
curlys = between (symbol "{%") (symbol "%}")

jsonParens :: Parser a -> Parser a
jsonParens = between (symbol "{") (symbol "}")

integer :: Parser Integer
integer = lexeme L.decimal

reservedWord :: String -> Parser ()
reservedWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedWords :: [String]
reservedWords = ["if", "endif", "for", "endfor", "assign", "capture", "endcapture", "query_graph", "in"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` reservedWords
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return x

whileParser :: Parser LiquidObject
whileParser = between spaceConsumer eof liquidObject

liquidObject :: Parser LiquidObject
liquidObject = f <$> sepBy1 liquidObject' spaceConsumer
  where
    f l = if length l == 1 then head l else Seq l

liquidObject' :: Parser LiquidObject
liquidObject' = parens liquidObject
  <|> curlys liquidObject
  <|> yamlPreamble
  <|> stringLiteral
  <|> assignStatement
  <|> captureStatement
  <|> jsonParens liquidObject

stringLiteral :: Parser LiquidObject
stringLiteral = do
  void (symbol "\"")
  value <- dbg "string" (some (alphaNumChar <|> char ' ' <|> char '_'))
  void (symbol "\"")
  return (StringLiteral value)

assignStatement :: Parser LiquidObject
assignStatement = do
  void (symbol "assign")
  var <- dbg "identifier" identifier
  void (symbol "=")
  expr <- stringLiteral
  return (Assign var expr)

yamlPreamble :: Parser LiquidObject
yamlPreamble = do
  void (symbol "---")
  value <- dbg "yaml" ((lexeme . try) (manyTill (L.charLiteral <|> newline) (symbol "---")))
  return (YAMLPreamble value)

captureStatement :: Parser LiquidObject
captureStatement = do
  void (symbol "capture")
  ident <- dbg "capture name" identifier
  void (symbol "%}")
  value <- dbg "capture value" liquidObject'
  void (symbol "{%")
  void (symbol "endcapture")
  return (Capture ident value)
