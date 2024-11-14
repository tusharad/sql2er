{-# LANGUAGE OverloadedStrings #-}

module ExamplesForTests where

import Data.Text (Text)
import Sql2er.Common.Types

foreginKeyStatement :: Table
foreginKeyStatement =
  Table
    { tableName = "summaries"
    , columns =
        [ Column
            { columnName = "summary_id"
            , columnType = PGserial
            , cConstraints = [PrimaryKey]
            }
        , Column
            { columnName = "thread_id"
            , columnType = PGinteger
            , cConstraints = [NotNull, ReferencesColumn "threads" (Just "thread_id")]
            }
        , Column {columnName = "summary_content", columnType = PGtext, cConstraints = [NotNull]}
        , Column
            { columnName = "summary_created_at"
            , columnType = PGtimestamp
            , cConstraints = [Default "now()"]
            }
        , Column
            { columnName = "summary_modified_at"
            , columnType = PGtimestamp
            , cConstraints = [Default "now()"]
            }
        ]
    , tableConstraints = []
    }

partitionConstraint :: Table
partitionConstraint =
  Table
    { tableName = "cities"
    , columns =
        [ Column
            { columnName = "city_id"
            , columnType = PGinteger
            , cConstraints = [NotNull]
            }
        , Column {columnName = "name", columnType = PGtext, cConstraints = [NotNull]}
        , Column {columnName = "population", columnType = PGbigInt, cConstraints = []}
        ]
    , tableConstraints = []
    }

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
        , ForeignKeyConstraint "sometable" "z" (Just "z")
        , CheckConstraint "z > 23"
        , UniqueConstraint ["y", "z"]
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
