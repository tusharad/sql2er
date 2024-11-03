{-# LANGUAGE OverloadedStrings #-}

module Sql2er.Parser.CreateType where

import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Sql2er.Common.Types
import Sql2er.Common.Utils

parseIdentifier :: Parser Text
parseIdentifier = lexeme (takeWhile1P Nothing (`notElem` (" \t\n,()" :: String)))

-- Parse CREATE TYPE statement
parseCreateType :: Parser CreateType
parseCreateType = (lexeme (string "create")) *> (lexeme (string "type")) *> do
  typeName <- parseIdentifier
  choice
    [ 
      parseEnumType typeName
    , parseCompositeType typeName
    , parseRangeType typeName
    , parseDefinedType typeName
    ]

-- Parse composite type
parseCompositeType :: Text -> Parser CreateType
parseCompositeType typeName = do
  _ <- lexeme (string "as")
  _ <- lexeme (char '(')
  attributes <- parseAttribute `sepBy` lexeme (char ',')
  _ <- lexeme (char ')')
  return $ CompositeType typeName attributes

-- Parse enum type
parseEnumType :: Text -> Parser CreateType
parseEnumType typeName = do
  _ <- lexeme (string "as") *> lexeme (string "enum")
  _ <- lexeme (char '(')
  labels <- lexeme parseQuotedString `sepBy` lexeme (char ',')
  _ <- lexeme (char ')')
  return $ EnumType typeName labels

-- Parse range type
parseRangeType :: Text -> Parser CreateType
parseRangeType typeName = do
  _ <- lexeme (string "as") *> lexeme (string "range")
  _ <- lexeme (char '(')
  attributes <- parseKeyValuePair `sepBy` lexeme (char ',')
  _ <- lexeme (char ')')
  return $ RangeType typeName attributes

-- Parse defined type
parseDefinedType :: Text -> Parser CreateType
parseDefinedType typeName = do
  _ <- lexeme (char '(')
  attributes <- lexeme parseKeyValuePair `sepBy` lexeme (char ',')
  _ <- lexeme (char ')')
  return $ DefinedType typeName attributes

-- Parse an attribute in key-value form
parseAttribute :: Parser (Text, Maybe Text)
parseAttribute = do
  name <- lexeme parseIdentifier
  dataType <- optional $ lexeme parseIdentifier
  _ <- optional (lexeme (string "collate") *> parseIdentifier) -- Optional COLLATE
  return (name, dataType)

-- Parse key-value pair
parseKeyValuePair :: Parser (Text, Text)
parseKeyValuePair = do
  key <- lexeme parseIdentifier
  _ <- lexeme (char '=')
  value <- parseIdentifier <|> parseQuotedString
  return (key, value)

-- Parse a quoted string
parseQuotedString :: Parser Text
parseQuotedString = char '\'' *> takeWhile1P Nothing (/= '\'') <* char '\''

-- Example usage: Parsing a CREATE TYPE statement
exampleUsage :: Text
exampleUsage = "CREATE TYPE my_enum AS ENUM ('one', 'two', 'three');"
