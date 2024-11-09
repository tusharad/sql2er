{-# LANGUAGE OverloadedStrings #-}

-- Module for parsing alter table commands
module Sql2er.Parser.AlterTable where

import Control.Monad
import Data.Maybe
import Sql2er.Common.Types
import Sql2er.Common.Utils
import Sql2er.Parser.Common
import Text.Megaparsec
import Text.Megaparsec.Char

parseRenameColumn :: Parser AlterTableAction
parseRenameColumn = do
  _ <- lexeme (string "rename") *> lexeme (string "column")
  oldCol <- parseWordForAlter
  _ <- lexeme (string "to")
  newCol <- parseWordForAlter
  return $ RenameColumn oldCol newCol

parseRenameConstraint :: Parser AlterTableAction
parseRenameConstraint = do
  _ <- lexeme (string "rename") *> lexeme (string "constraint")
  oldConstraintName <- parseWordForAlter
  _ <- lexeme (string "to")
  newConstraintName <- parseWordForAlter
  return $ RenameConstraint oldConstraintName newConstraintName

parseRenameTable :: Parser AlterTableAction
parseRenameTable = do
  _ <- lexeme (string "rename") *> lexeme (string "to")
  newName <- parseWordForAlter
  return $ RenameTable newName

parseSetSchema :: Parser AlterTableAction
parseSetSchema = do
  _ <- lexeme (string "set") *> lexeme (string "schema")
  schemaName <- parseWordForAlter
  return $ SetSchema schemaName

parseAddColumn :: Parser AlterTableAction
parseAddColumn = do
  _ <- lexeme (string "add") *> lexeme (string "column")
  colName <- lexeme parseWordForAlter
  dataType <- lexeme parseSqlType
  c <- many $ choice $ try <$> [
                      parsePrimaryKeyForCol
                    , parseNotNullForCol
                    , parseUniqueForCol
                    , parseDefaultVForCol
                    , parseReferenceForCol
                    , parseNullForCol 
                    , parseCheckForCol
                    ]
  t <- many (string "constraint" *> parseWordAndComma *> choice (try <$> [
                      parsePrimaryKeyForCol
                    , parseNotNullForCol
                    , parseUniqueForCol
                    , parseDefaultVForCol
                    , parseReferenceForCol
                    , parseNullForCol 
                    , parseCheckForCol
                    ]))
  return $
    AddColumn $
      Column
        { columnName = colName
        , columnType = dataType
        , cConstraints = c <> t
        }

parseDropColumn :: Parser AlterTableAction
parseDropColumn = do
  _ <- lexeme (string "drop") *> lexeme (string "column")
  colName <- parseWordForAlter
  cascade <- isJust <$> optional (string "cascade")
  _ <- optional (string "restrict")
  return $ DropColumn colName cascade

parseAlterColumnSetType :: Parser AlterTableAction
parseAlterColumnSetType = do
  _ <- lexeme (string "alter") *> lexeme (string "column")
  colName <- parseWordForAlter
  _ <- optional $ lexeme (string "set") *> lexeme (string "data")
  _ <- lexeme $ string "type"
  dataType <- lexeme parseSqlType
  mDefaultVal <-
    optional $
      lexeme (string "default") *> parseWordForAlter
  return $ AlterColumnSetType colName dataType mDefaultVal

parseAlterColumnSetDefault :: Parser AlterTableAction
parseAlterColumnSetDefault = do
  _ <- lexeme (string "alter") *> lexeme (string "column")
  colName <- parseWordForAlter
  _ <- lexeme (string "set") *> lexeme (string "default")
  defaultVal_ <- parseWordForAlter
  return $ AlterColumnSetDefault colName defaultVal_

parseAlterColumnDropDefault :: Parser AlterTableAction
parseAlterColumnDropDefault = do
  _ <- lexeme (string "alter") *> lexeme (string "column")
  colName <- parseWordForAlter
  _ <- lexeme (string "drop") *> lexeme (string "default")
  return $ AlterColunmnDropDefault colName

parseAlterColumnSetNotNull :: Parser AlterTableAction
parseAlterColumnSetNotNull = do
  _ <- lexeme (string "alter") *> lexeme (string "column")
  colName <- parseWordForAlter
  _ <- lexeme (string "set") *> lexeme (string "not") *> lexeme (string "null")
  return $ AlterColumnSetNotNull colName

parseAlterColumnDropNotNull :: Parser AlterTableAction
parseAlterColumnDropNotNull = do
  _ <- lexeme (string "alter") *> lexeme (string "column")
  colName <- parseWordForAlter
  _ <- lexeme (string "drop") *> lexeme (string "not") *> lexeme (string "null")
  return $ AlterColumnDropNotNull colName

parseAddTableConstraint :: Parser AlterTableAction
parseAddTableConstraint = do
  _ <- lexeme (string "add") 
  AddTableConstraint <$> parseTableConstraint

parseDropTableConstraint :: Parser AlterTableAction
parseDropTableConstraint = do
  _ <- lexeme (string "drop") *> lexeme (string "constraint")
  DropTableConstriant <$> (lexeme (takeWhile1P Nothing (`notElem` (" ,\t\n;" :: String))))

skipUnwantedParts :: Parser ()
skipUnwantedParts =
  void $
    many $
      try $
        choice
          [ skipTrigger
          , skipRule
          , skipRLSecurity
          , skipCluster
          , skipSetOptions
          , skipInherit
          , skipReplicaIdentity
          ]

skipTrigger :: Parser ()
skipTrigger = do
  _ <-
    string "disable trigger"
      <|> string "enable trigger"
      <|> string "enable replica trigger"
      <|> string "enable always trigger"
  _ <- takeWhileP Nothing (`notElem` ("\n;" :: String))
  return ()

skipRule :: Parser ()
skipRule = do
  _ <-
    string "disable rule"
      <|> string "enable rule"
      <|> string "enable replica rule"
      <|> string "enable always rule"
  _ <- takeWhileP Nothing (`notElem` ("\n;" :: String))
  return ()

skipRLSecurity :: Parser ()
skipRLSecurity = do
  _ <-
    string "disable row level security"
      <|> string "enable row level security"
      <|> string "force row level security"
      <|> string "no force row level security"
  return ()

skipCluster :: Parser ()
skipCluster = do
  _ <- string "cluster on" <|> string "set without cluster" <|> string "set without oids"
  _ <- takeWhileP Nothing (`notElem` (" \t\n;" :: String))
  return ()

skipSetOptions :: Parser ()
skipSetOptions = do
  _ <- string "set access method" <|> string "set tablespace" <|> string "set" <|> string "reset"
  _ <- takeWhileP Nothing (`notElem` ("\n;" :: String))
  return ()

skipInherit :: Parser ()
skipInherit = do
  _ <- string "inherit" <|> string "no inherit"
  _ <- parseWordForAlter
  return ()

skipReplicaIdentity :: Parser ()
skipReplicaIdentity = do
  _ <- string "replica identity"
  _ <- takeWhileP Nothing (`notElem` ("\n;" :: String))
  return ()

parseAlterTableAction :: Parser AlterTableAction
parseAlterTableAction =
  space
    *> choice
      ( try
          <$> [ parseRenameColumn
              , parseRenameConstraint
              , parseRenameTable
              , parseSetSchema
              , parseAddColumn
              , parseDropColumn
              , parseAlterColumnDropDefault
              , parseAlterColumnSetType
              , parseAlterColumnSetDefault
              , parseAlterColumnSetNotNull
              , parseAlterColumnDropNotNull
              , parseAddTableConstraint
              , parseDropTableConstraint
              ]
      )

parseAlterTable :: Parser AlterTable
parseAlterTable = do
  _ <- space *> lexeme (string "alter") *> lexeme (string "table")
  _ <- optional $ lexeme (string "if") *> lexeme (string "exists")
  _ <- optional $ lexeme (string "only")
  tName <- parseWordForAlter
  -- _ <- optional skipUnwantedParts
  AlterTable tName <$> parseAlterTableAction `sepBy1` lexeme (char ',')
