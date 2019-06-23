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

data Filter = FilterName String
  | ParameterizedFilterName String String
  deriving (Show, Eq)

data LiquidObject
  = LIdentifier String
  | Assign String String [Filter]
  | Capture String LiquidObject
  | StringLiteral String
  | YAMLPreamble String
  | Seq [LiquidObject]
  deriving (Show, Eq)

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
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '.' <|> char '[' <|> char ']' <|> char '\'')
    check x = if x `elem` reservedWords
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return x

stringLiteral :: Parser LiquidObject
stringLiteral = do
  void (symbol "\"")
  value <- (some (alphaNumChar <|> char ' ' <|> char '_'))
  void (symbol "\"")
  return (StringLiteral value)

getString :: Parser String
getString = (some (alphaNumChar <|> char ' ' <|> char '_'))

justString :: Parser String
justString = do
  void (symbol "\"")
  value <- getString
  void (symbol "\"")
  return value

assignement :: Parser LiquidObject
assignement = do
  void (symbol "assign")
  var <- identifier
  void (symbol "=")
  expr <- (justString <|> identifier)
  skipMany $ symbol "| "
  filters <- filterCall `sepBy` (symbol "| ")
  return (Assign var expr filters)

filterCall :: Parser Filter
filterCall = try filterWithParam <|> filterWithoutParam

filterWithParam :: Parser Filter
filterWithParam = do
  name <- identifier
  void (symbol ":")
  parameter <- (some (alphaNumChar <|> char ' ' <|> char '_' <|> char '\'' <|> char ',' <|> char '.'))
  return (ParameterizedFilterName name parameter)

filterWithoutParam :: Parser Filter
filterWithoutParam = do
  name <- identifier
  return (FilterName name)

yamlPreamble :: Parser LiquidObject
yamlPreamble = do
  void (symbol "---")
  value <- ((lexeme . try) (manyTill (L.charLiteral <|> newline) (symbol "---")))
  return (YAMLPreamble value)

captureStatement :: Parser LiquidObject
captureStatement = do
  void (symbol "capture")
  ident <- identifier
  void (symbol "%}")
  value <- liquidObject'
  void (symbol "{%")
  void (symbol "endcapture")
  return (Capture ident value)

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
  <|> assignement
  <|> captureStatement
  <|> jsonParens liquidObject


