{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Sql2er.Mermaid where

import Data.Text (Text)
import Data.Text qualified as T
import Sql2er.Common.Types

-- Function to convert SqlType to a simplified string representation for Mermaid
sqlTypeToString :: SqlType -> Text
sqlTypeToString PGbigInt = "bigint"
sqlTypeToString PGint8 = "int8"
sqlTypeToString PGbit = "bit"
sqlTypeToString PGboolean = "boolean"
sqlTypeToString PGbox = "box"
sqlTypeToString PGbytea = "bytea"
sqlTypeToString (PGchar _) = "char"
sqlTypeToString (PGvarchar _) = "varchar"
sqlTypeToString PGdate = "date"
sqlTypeToString PGinteger = "integer"
sqlTypeToString PGtext = "text"
sqlTypeToString PGserial = "serial"
sqlTypeToString PGserial4 = "serial4"
sqlTypeToString PGint = "int"
sqlTypeToString PGtimestamp = "timestamp"
sqlTypeToString PGtimestamptz = "timestamptz"
sqlTypeToString PGBigSerial = "bigserial"
sqlTypeToString SomeType = "unknown"

-- Function to convert a Column to Mermaid's attribute representation
columnToMermaid :: Column -> Text
columnToMermaid Column {columnName, columnType} =
  "    " <> sqlTypeToString columnType <> " " <> columnName

-- Function to convert a Table to Mermaid's table representation
tableToMermaid :: Table -> Text
tableToMermaid Table {tableName, columns} =
  T.unlines $
    [tableName <> " {"] ++ map columnToMermaid columns ++ ["}"]

-- Function to create relationship representation for ForeignKeyConstraint
foreignKeyToMermaid :: TableName -> TableConstraint -> [Text]
foreignKeyToMermaid tableName (ForeignKeyConstraint targetTable targetColumn _) =
  [tableName <> " ||--o{ " <> targetTable <> " : " <> targetColumn]
foreignKeyToMermaid _ _ = []

-- Function to process a single Table to produce Mermaid diagram parts
processTable :: Table -> [Text]
processTable t@Table {tableName, tableConstraints} =
  let tableDef = tableToMermaid t
      relations = concatMap (foreignKeyToMermaid tableName) tableConstraints
   in tableDef : relations

-- Main function to process all tables and generate the Mermaid ERD
generateMermaidERD :: [Table] -> Text
generateMermaidERD tables =
  "erDiagram\n" <> T.unlines (concatMap processTable tables)