{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Liquid where

import           Control.Monad                  (void)
import           Control.Monad.Combinators.Expr
import qualified Data.Text                      as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Debug

default (T.Text)

data Filter = FilterName T.Text
  | ParameterizedFilterName T.Text T.Text
  deriving (Show, Eq)

data HtmlAttribute = HtmlAttribute T.Text
  deriving (Show, Eq)

data LiquidObject
  = LIdentifier T.Text
  | Assign T.Text T.Text [Filter]
  | Capture T.Text LiquidObject
  | HtmlTag T.Text HtmlAttribute LiquidObject
  | IncludeForm T.Text
  | GraphQLQuery T.Text T.Text
  | StringLiteral T.Text
  | YAMLPreamble T.Text
  | Seq [LiquidObject]
  deriving Eq


instance Show LiquidObject where
  show (LIdentifier i) = "Identifier: " ++ (show i) ++ "\n"
  show (Assign variable value filters) = "Assignement variable: " ++ (T.unpack variable) ++ " value: " ++ (T.unpack value) ++ " filters: " ++ (concatMap show filters) ++ "\n"
  show (Capture name content) = "Capture block named: " ++ (T.unpack name) ++ " " ++ (show content) ++ "\n"
  show (HtmlTag name attribute content) = "Html tag: " ++ (T.unpack name) ++ " " ++ " attributes: " ++ " " ++ (show content) ++ "\n"
  show (IncludeForm formName) = "Include html form: " ++ (T.unpack formName) ++ "\n"
  show (GraphQLQuery _ queryName) = "GraphQL query: " ++ (T.unpack queryName) ++ "\n"
  show (StringLiteral text) = (T.unpack text)
  show (YAMLPreamble text) = (T.unpack text)
  show (Seq objects) = "\n\n This page content: \n " ++ (concatMap show objects)

type Parser = Parsec Void T.Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipBlockComment "{% comment %}" "{% endcomment %}"
    blockCmnt = L.skipBlockComment "{% comment %}" "{% endcomment %}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol' spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "{{") (symbol "}}")

curlys :: Parser a -> Parser a
curlys = between (symbol "{%") (symbol "%}")

curlysDash :: Parser a -> Parser a
curlysDash = between (symbol "{%-") (symbol "-%}")

jsonParens :: Parser a -> Parser a
jsonParens = between (symbol "{") (symbol "}")

integer :: Parser Integer
integer = lexeme L.decimal

reservedWord :: T.Text -> Parser ()
reservedWord w = lexeme . try $ (string w *> notFollowedBy alphaNumChar)

reservedWords :: [T.Text]
reservedWords = ["if", "endif", "for", "endfor", "assign", "capture", "endcapture", "query_graph", "in"]

identifier :: Parser T.Text
identifier = lexeme . try $ (p >>= check)
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '.' <|> char '[' <|> char ']' <|> char '\'')
    check x = if (T.pack x) `elem` reservedWords
                 then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                 else return (T.pack x)

getString :: Parser [Char]
getString = lexeme . try $ ((:) <$> alphaNumChar <*> many (alphaNumChar <|> char ' ' <|> char '_'))

stringValue :: Parser T.Text
stringValue = do
  opening <- eitherP (symbol "\"") (symbol "'")
  value <- getString
  case opening of
    Left qt   -> void $ symbol qt
    Right qtt -> void $ symbol qtt
  return (T.pack(value))

stringLiteral :: Parser LiquidObject
stringLiteral = do
  value <- stringValue
  return (StringLiteral value)

imTryParse :: T.Text -> Either (ParseErrorBundle s e) LiquidObject  -> LiquidObject
imTryParse b = extract
  where
    extract (Left e)  = StringLiteral b
    extract (Right a) = a

htmlTagWithAttributes :: Parser LiquidObject
htmlTagWithAttributes = do
  void (symbol "<" <* notFollowedBy (symbol "/"))
  tagName <- (manyTill (L.charLiteral) (symbol " " <* notFollowedBy (symbol "<")))
  tagAttributes <- manyTill L.charLiteral (symbol ">")
  tagBody <- (lexeme . try) (manyTill (L.charLiteral <|> newline) (symbol ("</" <> (T.pack tagName))))
  void (symbol ">")
  return (HtmlTag (T.pack tagName) (HtmlAttribute (T.pack tagAttributes)) (imTryParse (T.pack tagBody) (parse whileParser "" (T.pack tagBody))))

htmlTagWithoutAttributes :: Parser LiquidObject
htmlTagWithoutAttributes = do
  void (symbol "<" <* notFollowedBy (symbol "/"))
  tagName <- (manyTill (L.charLiteral) (symbol ">"))
  tagBody <- (lexeme . try) (manyTill (L.charLiteral <|> newline) (symbol ("</" <> (T.pack tagName))))
  void (symbol ">")
  return (HtmlTag (T.pack tagName) (HtmlAttribute "") (imTryParse (T.pack tagBody) (parse whileParser "" (T.pack tagBody))))

htmlTag :: Parser LiquidObject
htmlTag = try htmlTagWithAttributes <|> htmlTagWithoutAttributes

assignement :: Parser LiquidObject
assignement = do
  void (symbol "assign")
  var <- identifier
  void (symbol "=")
  expr <- (stringValue <|> identifier)
  skipMany $ symbol "| "
  filters <- filterCall `sepBy` (symbol "| ")
  return (Assign var expr filters)

filterCall :: Parser Filter
filterCall = try filterWithParam <|> filterWithoutParam

filterWithParam :: Parser Filter
filterWithParam = do
  name <- identifier
  void (symbol ":")
  parameter <- some (alphaNumChar <|> char ' ' <|> char '_' <|> char '\'' <|> char ',' <|> char '.')
  return (ParameterizedFilterName name (T.pack parameter))

filterWithoutParam :: Parser Filter
filterWithoutParam = do
  name <- identifier
  return (FilterName name)

includeForm :: Parser LiquidObject
includeForm = do
  void $ symbol "include_form"
  path <- (stringValue <|> identifier)
  return (IncludeForm path)

executeGraphql :: Parser LiquidObject
executeGraphql = do
  void $ symbol "graphql"
  ident <- identifier
  void $ symbol "="
  query_name <- (stringValue <|> identifier)
  return (GraphQLQuery ident query_name)

yamlPreamble :: Parser LiquidObject
yamlPreamble = do
  void $ symbol "---"
  value <- lexeme . try $ manyTill (L.charLiteral <|> newline) (symbol "---")
  return (YAMLPreamble (T.pack value))

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
  <|> curlysDash liquidObject
  <|> curlys liquidObject
  <|> yamlPreamble
  <|> stringLiteral
  <|> assignement
  <|> captureStatement
  <|> includeForm
  <|> executeGraphql
  <|> jsonParens liquidObject
  <|> htmlTag

parseChars :: [Char] -> Either (ParseErrorBundle T.Text Data.Void.Void) LiquidObject
parseChars chars = parse whileParser "" (T.pack chars)
