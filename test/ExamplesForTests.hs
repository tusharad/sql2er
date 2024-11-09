{-# LANGUAGE OverloadedStrings #-}

module ExamplesForTests where

import Data.Text (Text)
import Sql2er.Common.Types

tableConstraint :: Table
tableConstraint =
  Table
    { tableName = "x"
    , columns =
        [ Column
            { columnName = "y"
            , columnType = PGvarchar (Just 3)
            , cConstraints = [Unique]
            }
        , Column {columnName = "z", columnType = PGinteger, cConstraints = []}
        ]
    , tableConstraints =
        [ UniqueConstraint ["z"]
        , PrimaryKeyConstraint "y"
        , ForeignKeyConstraint "z" "sometable" (Just "z")
        , CheckConstraint "z > 23"
        , UniqueConstraint ["y","z"]
        ]
    }

simpleTable1String :: Text
simpleTable1String =
  "CREATE TABLE films (\n\
  \code        char(5) CONSTRAINT firstkey PRIMARY KEY,\
  \title       varchar(40) NOT NULL,\
  \did         integer NOT NULL,\
  \date_prod   date,\
  \kind        varchar(10),\
  \);"

uniqueConstraint :: Table
uniqueConstraint =
  Table
    { tableName = "x"
    , columns =
        [ Column
            { columnName = "y"
            , columnType = PGvarchar (Just 3)
            , cConstraints = [Unique]
            }
        , Column
            { columnName = "z"
            , columnType = PGinteger
            , cConstraints = []
            }
        ]
    , tableConstraints = [UniqueConstraint ["z"]]
    }

columnNullNotNull :: Table
columnNullNotNull =
  Table
    { tableName = "x"
    , columns =
        [ Column
            { columnName = "y"
            , columnType = PGvarchar (Just 3)
            , cConstraints = [NotNull]
            }
        , Column
            { columnName = "z"
            , columnType = PGinteger
            , cConstraints = [Null]
            }
        ]
    , tableConstraints = []
    }

constraintNamePK :: Table
constraintNamePK =
  Table
    { tableName = "x"
    , columns =
        [ Column
            { columnName = "y"
            , columnType = PGvarchar (Just 3)
            , cConstraints = [PrimaryKey]
            }
        , Column
            { columnName = "z"
            , columnType = PGinteger
            , cConstraints = []
            }
        ]
    , tableConstraints = []
    }

simpleTable0 :: Table
simpleTable0 =
  Table
    { tableName = "x"
    , columns =
        [ Column
            { columnName = "y"
            , columnType = PGinteger
            , cConstraints = []
            }
        ]
    , tableConstraints = []
    }
