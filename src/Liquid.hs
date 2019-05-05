{-# LANGUAGE OverloadedStrings #-}

module Liquid where

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

data LObject
  = LIdentifier String
  | Assign String LObject
  | StringLiteral String
  | YAMLPreamble String
  | Seq [LObject]
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

integer :: Parser Integer
integer = lexeme L.decimal

reservedWord :: String -> Parser ()
reservedWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedWords :: [String]
reservedWords = ["if", "endif", "for", "endfor", "assign"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x = if x `elem` reservedWords
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return x

whileParser :: Parser LObject
whileParser = between spaceConsumer eof lObject

lObject :: Parser LObject
lObject = f <$> sepBy1 lObject' spaceConsumer
  where
    f l = if length l == 1 then head l else Seq l

lObject' :: Parser LObject
lObject' = parens lObject
  <|> curlys lObject
  <|> yamlPreamble
  <|> stringLiteral
  <|> assignStatement

stringLiteral :: Parser LObject
stringLiteral = do
  void (symbol "\"")
  value <- some (alphaNumChar <|> char ' ')
  void (symbol "\"")
  return (StringLiteral value)

assignStatement :: Parser LObject
assignStatement = do
  void (symbol "assign")
  var <- identifier
  void (symbol "=")
  expr <- stringLiteral
  return (Assign var expr)

yamlPreamble :: Parser LObject
yamlPreamble = do
  void (symbol "---")
  value <- (lexeme . try) (manyTill (L.charLiteral <|> newline) (symbol "---"))
  return (YAMLPreamble value)
