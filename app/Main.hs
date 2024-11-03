{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Sql2er.Common.Types
import Sql2er.Parser (parseSqlScript)
import System.Exit (exitFailure)
import Text.Megaparsec
import Sql2er.ERDiagram (renderErDiagram)
import Sql2er.CmdArgs (Options(..), parseCmdArgs)

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

main :: IO ()
main = do
  opts <- parseCmdArgs
  content <- T.toLower
        <$> T.readFile (sqlFile opts)
          `catch` ( \e -> do
                      print (e :: SomeException)
                      exitFailure
                  )
  case parse parseSqlScript "sql parser" content of
    Left _ -> putStrLn "parsing failed :("
    Right r -> do
      let tableList = postParsingSetup r []
      renderErDiagram 
        (outputFile opts)
        (width opts)
        (height opts)
        tableList
