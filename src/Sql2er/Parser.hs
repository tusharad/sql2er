{-# LANGUAGE OverloadedStrings #-}
module Sql2er.Parser (parseSql, parseSqlScript, parseScript, postParsingSetup) where

import Data.List (find)
import Data.Maybe
import Data.Text qualified as T
import Sql2er.Common.Types
import Sql2er.Common.Utils (ignoreStatement, lexeme)
import Sql2er.Mermaid
import Sql2er.Parser.AlterTable (parseAlterTable)
import Sql2er.Parser.CreateTable (parseCreateTable)
import Sql2er.Parser.CreateType (parseCreateType)
import Sql2er.Parser.DropTable (parseDropTable)
import Text.Megaparsec
import Text.Megaparsec.Char

-- This function will find all the create table statements,
-- alter or drop the tables based on statements.
postParsingSetup :: [Statement] -> [Table] -> [Table]
postParsingSetup [] r = r
postParsingSetup (statement : statements) resTable = do
  case statement of
    AlterStatement alterStatement -> do
      let alteredTableName = alterTableName alterStatement
      let actions = action alterStatement
      case find (\x -> alteredTableName == tableName x) resTable of
        Nothing -> postParsingSetup statements resTable -- Table name of alter statement not found
        Just alteringTable -> do
          let res = applyAlteration alteringTable actions
              newList = removeElem alteringTable resTable
          postParsingSetup statements (res : newList)
    DropStatement dropStatement -> postParsingSetup statements (dropTables (tableNames dropStatement) resTable)
    CreateStatement s -> postParsingSetup statements (s : resTable)
    CreateTypeStatement _ -> postParsingSetup statements resTable
    EOF -> postParsingSetup statements resTable
  where
    dropTables :: [TableName] -> [Table] -> [Table]
    dropTables tNames = filter (\t -> tableName t `notElem` tNames)

    applyAlterAction :: Table -> AlterTableAction -> Table
    applyAlterAction alteringTable act = do
      case act of
        RenameColumn oldCol newCol -> do
          case find (\x -> oldCol == columnName x) (columns alteringTable) of
            Nothing -> alteringTable
            Just col -> do
              let listWithoutElem = removeElem col (columns alteringTable)
                  listWithNewElem = (col {columnName = newCol}) : listWithoutElem
              alteringTable {columns = listWithNewElem}
        AddColumn col -> alteringTable {columns = col : columns alteringTable}
        DropColumn col _ -> do
          case find (\x -> col == columnName x) (columns alteringTable) of
            Nothing -> alteringTable
            Just col0 -> alteringTable {columns = removeElem col0 (columns alteringTable)}
        RenameTable newTableName -> alteringTable {tableName = newTableName}
        SetSchema schemaName -> alteringTable {tableName = schemaName <> "." <> tableName alteringTable}
        AlterColumnSetType colName sqlType mDefaultVal -> do
          case find (\x -> colName == columnName x) (columns alteringTable) of
            Nothing -> alteringTable
            Just col -> do
              let listWithoutElem = removeElem col (columns alteringTable)
              let listWithNewElem =
                    case mDefaultVal of
                      Nothing -> (col {columnType = sqlType}) : listWithoutElem
                      Just dVal -> do
                        let newCConstraints =
                              removeElemP
                                ( \x ->
                                    case x of
                                      (Default _) -> True
                                      _ -> False
                                )
                                (cConstraints col)
                        ( col
                            { columnType = sqlType
                            , cConstraints = Default dVal : newCConstraints
                            }
                          )
                          : listWithoutElem
              alteringTable {columns = listWithNewElem}
        AlterColumnSetDefault colName dVal -> do
          case find (\x -> colName == columnName x) (columns alteringTable) of
            Nothing -> alteringTable
            Just col -> do
              let listWithoutElem = removeElem col (columns alteringTable)
              let newCConstraints =
                    removeElemP
                      ( \x ->
                          case x of
                            (Default _) -> True
                            _ -> False
                      )
                      (cConstraints col)
              let listWithNewElem = col {cConstraints = Default dVal : newCConstraints} : listWithoutElem
              alteringTable {columns = listWithNewElem}
        AlterColunmnDropDefault colName -> do
          case find (\x -> colName == columnName x) (columns alteringTable) of
            Nothing -> alteringTable
            Just col -> do
              let listWithoutElem = removeElem col (columns alteringTable)
              let newCConstraints =
                    removeElemP
                      ( \x ->
                          case x of
                            (Default _) -> True
                            _ -> False
                      )
                      (cConstraints col)
              alteringTable {columns = (col {cConstraints = newCConstraints}) : listWithoutElem}
        AlterColumnSetNotNull colName -> do
          case find (\x -> colName == columnName x) (columns alteringTable) of
            Nothing -> alteringTable
            Just col -> do
              let listWithoutElem = removeElem col (columns alteringTable)
              alteringTable
                { columns = (col {cConstraints = NotNull : (cConstraints col)}) : listWithoutElem
                }
        AlterColumnDropNotNull colName -> do
          case find (\x -> colName == columnName x) (columns alteringTable) of
            Nothing -> alteringTable
            Just col -> do
              let listWithoutElem = removeElem col (columns alteringTable)
              let newCConstraints =
                    removeElemP
                      ( \x ->
                          case x of
                            NotNull -> True
                            _ -> False
                      )
                      (cConstraints col)
              alteringTable
                { columns = (col {cConstraints = newCConstraints}) : listWithoutElem
                }
        AddTableConstraint tConstraint ->
          alteringTable
            { tableConstraints = tConstraint : tableConstraints alteringTable
            }
        DropTableConstriant _ -> alteringTable -- Cannot implement since we are not tracking constraint name
        RenameConstraint _ _ -> alteringTable -- Cannot implement since we are not tracking constraint name
    applyAlteration :: Table -> [AlterTableAction] -> Table
    applyAlteration = foldl applyAlterAction

removeElemP :: (a -> Bool) -> [a] -> [a]
removeElemP p lst = helper lst []
  where
    helper [] res = res
    helper (x : xs) res = if p x then res ++ xs else helper xs (x : res)

removeElem :: Eq a => a -> [a] -> [a]
removeElem ele lst = helper lst []
  where
    helper [] res = res
    helper (x : xs) res = if ele == x then res ++ xs else helper xs (x : res)

parseSql :: Parser Statement
parseSql =
  choice
    [ try $ CreateTypeStatement <$> parseCreateType
    , try $ CreateStatement <$> parseCreateTable
    , try $ AlterStatement <$> parseAlterTable
    , try $ DropStatement <$> parseDropTable
    , EOF <$ eof
    ]

parseSqlScript :: Parser [Statement]
parseSqlScript = do
  res <- (Nothing <$ ignoreStatement <|> Just <$> parseSql) `sepBy` lexeme (char ';')
  return $ catMaybes res

parseScript :: String -> Either String String
parseScript t =
  case parse parseSqlScript "" (T.toLower $ T.pack t) of
    Left _ -> Left "invalid"
    Right r -> do
      let finalizedTables = postParsingSetup r []
      Right $ T.unpack $ generateMermaidERD finalizedTables 
