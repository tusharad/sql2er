{-# LANGUAGE OverloadedStrings #-}

-- Common parsring functions between modules
module Sql2er.Parser.Common where

import Data.Text (Text)
import Sql2er.Common.Types
import Sql2er.Common.Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

parseWord :: Parser Text
parseWord = lexeme (takeWhile1P Nothing (`notElem` (" \t\n;" :: String)))

parseWordAndComma :: Parser Text
parseWordAndComma = lexeme (takeWhile1P Nothing (`notElem` (" \t\n,);" :: String)))

parseWordWithoutParenthesis :: Parser Text
parseWordWithoutParenthesis = lexeme (takeWhile1P Nothing (`notElem` (" \t\n,;" :: String)))

parseWordForAlter :: Parser Text
parseWordForAlter = lexeme (takeWhile1P Nothing (`notElem` (" \t\n;," :: String)))

parseDefaultValue :: Parser Text
parseDefaultValue =
  lexeme (char '\'' *> takeWhile1P Nothing (/= '\'') <* char '\'') <|> parseWordWithoutParenthesis

parseTableConstraint :: Parser TableConstraint
parseTableConstraint = do
  _ <- optional $ lexeme (string "constraint") *> parseWord
  choice $
    try
      <$> [ foreignKeyConstraint
          , uniqueConstraint
          , primaryKeyConstraint
          , checkConstraint
          , excludeConstraint
          ]

uniqueConstraint :: Parser TableConstraint
uniqueConstraint = do
  _ <- lexeme (string "unique")
  cols <-
    between
      (char '(')
      (char ')')
      ( lexeme
          ( takeWhile1P
              Nothing
              (`notElem` (" \t\n;,)" :: String))
          )
          `sepBy1` lexeme (char ',')
      )
  return $ UniqueConstraint cols

primaryKeyConstraint :: Parser TableConstraint
primaryKeyConstraint = do
  _ <- lexeme (string "primary") *> lexeme (string "key")
  col <- between (char '(') (char ')') (takeWhile1P Nothing (/= ')'))
  return $ PrimaryKeyConstraint col

checkConstraint :: Parser TableConstraint
checkConstraint = do
  _ <- lexeme (string "check")
  expr <- lexeme (takeWhile1P Nothing (`notElem` (";," :: String)))
  return $ CheckConstraint expr

foreignKeyConstraint :: Parser TableConstraint
foreignKeyConstraint = do
  _ <- lexeme (string "foreign") *> lexeme (string "key")
  col <- between (lexeme (char '(')) (lexeme (char ')')) (takeWhile1P Nothing (/= ')'))
  _ <- lexeme (string "references")
  refTable <- parseWord
  refColumn <- optional (between (char '(') (char ')') (takeWhile1P Nothing (/= ')')))
  return $ ForeignKeyConstraint col refTable refColumn

excludeConstraint :: Parser TableConstraint
excludeConstraint = do
  _ <- string "exclude"
  expr <- between (char '(') (char ')') (takeWhile1P Nothing (/= ')'))
  return $ ExcludeConstraint expr

parseDefaultVal :: Parser Text
parseDefaultVal =
  lexeme $
    string "now()"
      <|> string "true"
      <|> string "false"
      <|> (char '\'' *> takeWhile1P Nothing (/= '\'') <* char '\'')
      <|> (char '\'' *> takeWhile1P Nothing (/= '\"') <* char '\"')
      <|> takeWhile1P Nothing (`notElem` (" " :: String))

parsePrimaryKeyForCol :: Parser ColumnConstraint
parsePrimaryKeyForCol = PrimaryKey <$ (lexeme (string "primary") *> lexeme (string "key"))

parseNotNullForCol :: Parser ColumnConstraint
parseNotNullForCol = NotNull <$ (lexeme (string "not") *> lexeme (string "null"))

parseNullForCol :: Parser ColumnConstraint
parseNullForCol = Null <$ lexeme (string "null")

parseUniqueForCol :: Parser ColumnConstraint
parseUniqueForCol = Unique <$ lexeme (string "unique")

parseDefaultVForCol :: Parser ColumnConstraint
parseDefaultVForCol = Default <$> (lexeme (string "default") *> parseDefaultVal)

parseReferenceForCol :: Parser ColumnConstraint
parseReferenceForCol = do
  _ <- lexeme (string "references")
  refTable <- parseWord
  refCol <- optional (lexeme (char '(') *> lexeme (takeWhile1P Nothing (/= ')')) <* lexeme (char ')'))
  _ <-
    try $
      optional $
        lexeme (string "on")
          *> (lexeme (string "delete") <|> lexeme (string "update"))
          *> takeWhile1P Nothing (`notElem` (" \t\n;,)" :: String))
  return (ReferencesColumn refTable refCol)

parseSqlType :: Parser SqlType
parseSqlType =
  choice $
    try
      <$> [ PGbit
              <$ string "bit"
          , PGint8
              <$ string "int8"
          , PGboolean
              <$ string "boolean"
          , PGbox
              <$ string "box"
          , PGbytea
              <$ string "bytea"
          , PGchar
              <$ string "char"
          , PGvarchar
              <$> (string "varchar" *> optional (space *> lexeme (char '(') *> decimal <* char ')'))
          , PGdate
              <$ string "date"
          , PGinteger
              <$ (string "integer" <|> string "int")
          , PGtext
              <$ string "text"
          , PGserial4
              <$ string "serial4"
          , PGserial
              <$ string "serial"
          , PGtimestamptz <$ lexeme (string "timestamptz")
          , PGtimestamp
              <$ lexeme (string "timestamp")
              <* optional (lexeme (string "without") *> lexeme (string "time") *> lexeme (string "zone"))
          , SomeType <$ lexeme (takeWhile1P Nothing (/= ' '))
          ]
