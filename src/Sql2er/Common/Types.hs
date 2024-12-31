module Sql2er.Common.Types
  ( TableName
  , ColumnName
  , Parser
  , Column (..)
  , Table (..)
  , SqlType (..)
  , TableConstraint (..)
  , AlterTable (..)
  , AlterTableAction (..)
  , DropTable (..)
  , DropTableOption (..)
  , Statement (..)
  , CreateType (..)
  , ColumnConstraint (..)
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type TableName = Text
type ColumnName = Text
type Parser = Parsec Void Text

data SqlType
  = PGbigInt
  | PGint8
  | PGbit
  | PGboolean
  | PGbox
  | PGbytea
  | PGchar (Maybe Int)
  | PGvarchar (Maybe Int)
  | PGdate
  | PGinteger
  | PGtext
  | PGserial
  | PGserial4
  | PGint
  | PGtimestamp
  | PGtimestamptz
  | PGBigSerial
  | PGJsonb
  | SomeType
  deriving (Show, Eq)

data ColumnConstraint = 
    PrimaryKey 
    | Unique 
    | NotNull 
    | Null
    | Default Text
    | ReferencesColumn TableName (Maybe ColumnName)
    | Check Text
    deriving (Show, Eq)

data Column = Column
  { columnName :: ColumnName
  , columnType :: SqlType
  , cConstraints :: [ColumnConstraint]
  }
  deriving (Show, Eq)

data TableConstraint
  = UniqueConstraint [ColumnName]
  | PrimaryKeyConstraint ColumnName
  | CheckConstraint Text
  | ForeignKeyConstraint TableName ColumnName (Maybe Text)
  | ExcludeConstraint Text
  deriving (Eq, Show)

data Table = Table
  { tableName :: TableName
  , columns :: [Column]
  , tableConstraints :: [TableConstraint]
  }
  deriving (Show, Eq)

data AlterTableAction
  = RenameColumn
      ColumnName -- old column name
      ColumnName -- new column name
  | RenameConstraint Text Text
  | RenameTable TableName -- new table name
  | SetSchema Text
  | AddColumn Column
  | DropColumn ColumnName Bool -- Cascade or not
  | AlterColumnSetType ColumnName SqlType (Maybe Text)
  | AlterColumnSetDefault ColumnName Text
  | AlterColunmnDropDefault ColumnName
  | AlterColumnSetNotNull ColumnName
  | AlterColumnDropNotNull ColumnName
  | AddTableConstraint TableConstraint
  | DropTableConstriant Text
  deriving (Show, Eq)

data AlterTable = AlterTable
  { alterTableName :: TableName
  , action :: [AlterTableAction]
  }
  deriving (Show, Eq)

data DropTableOption = Cascade | Restrict deriving (Show, Eq)

data DropTable = DropTable
  { tableNames :: [TableName]
  , dropOption :: Maybe DropTableOption
  }
  deriving (Show, Eq)

data Statement
  = CreateStatement Table
  | AlterStatement AlterTable
  | DropStatement DropTable
  | CreateTypeStatement CreateType
  | EOF
  deriving (Show, Eq)

data CreateType
  = CompositeType Text [(Text, Maybe Text)] -- Type name, list of (attribute name, data type)
  | EnumType Text [Text] -- Type name, list of labels
  | RangeType Text [(Text, Text)] -- Type name, list of (attribute, value)
  | DefinedType Text [(Text, Text)] -- Type name, list of (attribute, value)
  deriving (Show, Eq)
