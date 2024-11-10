{-# LANGUAGE OverloadedStrings #-}

module Sql2er.Parser.CreateTable where

import Control.Monad
import Sql2er.Common.Types
import Sql2er.Common.Utils
import Sql2er.Parser.Common
import Text.Megaparsec
import Text.Megaparsec.Char

parseColumn :: Parser Column
parseColumn = do
  cName <- parseWordAndComma
  sqlType <- lexeme parseSqlType
  c <-
    many $
      choice $
        try
          <$> [ parsePrimaryKeyForCol
              , parseNotNullForCol
              , parseUniqueForCol
              , parseDefaultVForCol
              , parseReferenceForCol
              , parseNullForCol
              , parseCheckForCol
              ]
  t <-
    many
      ( lexeme (string "constraint")
          *> parseWordAndComma
          *> choice
            ( try
                <$> [ parsePrimaryKeyForCol
                    , parseNotNullForCol
                    , parseUniqueForCol
                    , parseDefaultVForCol
                    , parseReferenceForCol
                    , parseNullForCol
                    , parseCheckForCol
                    ]
            )
      )
  void $ optional ignoreConstraints
  return
    Column
      { columnName = cName
      , columnType = sqlType
      , cConstraints = c <> t
      }

-- Helper parsers for skipping unwanted parts
skipOptionalParts :: Parser ()
skipOptionalParts =
  void $
    many $
      try $
        choice
          [ skipLikeClause
          , skipPartitioning
          , skipInheritsClause
          , skipUsingMethod
          , -- , skipWithOrWithoutOids
            skipOnCommit
          , skipTablespace
          ]

skipLikeClause :: Parser ()
skipLikeClause = do
  _ <- lexeme $ string "like"
  _ <- takeWhile1P Nothing (/= ';')
  return ()

skipPartitioning :: Parser ()
skipPartitioning = do
  _ <- lexeme (string "partition") *> lexeme (string "by")
  _ <- takeWhile1P Nothing (/= ';')
  return ()

skipInheritsClause :: Parser ()
skipInheritsClause = do
  _ <- lexeme $ string "inherits"
  _ <- takeWhile1P Nothing (/= ';')
  return ()

skipUsingMethod :: Parser ()
skipUsingMethod = do
  _ <- lexeme $ string "using"
  _ <- takeWhile1P Nothing (/= ';')
  return ()

skipWithOrWithoutOids :: Parser ()
skipWithOrWithoutOids = undefined

skipOnCommit :: Parser ()
skipOnCommit = do
  _ <- string "on commit"
  _ <-
    lexeme (string "preserve")
      *> lexeme (string "rows")
        <|> lexeme (string "delete")
      *> lexeme (string "rows")
        <|> lexeme (string "drop")
  return ()

skipTablespace :: Parser ()
skipTablespace = do
  _ <- lexeme $ string "tablespace"
  _ <- takeWhile1P Nothing (/= ';')
  return ()

parseColumnOrConstraint :: Parser (Either Column TableConstraint)
parseColumnOrConstraint =
  (Right <$> lexeme parseTableConstraint)
    <|> (Left <$> lexeme parseColumn)

parseCreateTable :: Parser Table
parseCreateTable = do
  _ <- space <* lexeme (string "create")
  _ <- optional (try (string "global") <|> try (string "local"))
  _ <- optional (string "temporary" <|> string "temp")
  _ <- optional (string "unlogged")
  _ <- lexeme (string "table")
  _ <- optional $ lexeme (string "if") *> lexeme (string "not") *> lexeme (string "exists")
  tName <- lexeme (takeWhile1P Nothing (`notElem` (" \t\n)(" :: String)))
  items <-
    between
      (lexeme (char '('))
      (lexeme (char ')'))
      (parseColumnOrConstraint `sepBy` lexeme (char ','))
  _ <- optional $ lexeme skipOptionalParts
  let cols = [c | Left c <- items]
  let constraints = [con | Right con <- items]
  return Table {tableName = tName, columns = cols, tableConstraints = constraints}
