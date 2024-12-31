{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import ExamplesForTests
import ExamplesForTests qualified as Ex
import Sql2er.Common.Types
import Sql2er.Common.Utils (begin, commit, rollback)
import Sql2er.Mermaid (generateMermaidERD)
import Sql2er.Parser
import Sql2er.Parser.CreateTable (parseCreateTable)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

testParse :: Text -> Parser Table -> Table -> String -> Assertion
testParse inputText parsingFunc expectedOp failureMsg = do
  let eRes =
        runParser
          parsingFunc
          ""
          inputText
  case eRes of
    Left _ -> assertFailure failureMsg
    Right r -> expectedOp @=? r

testBeginCommit :: TestTree
testBeginCommit =
  testGroup
    "Begin commit"
    [ testCase "test parse begin" $ do
        let eRes = runParser begin "" (T.toLower "BEGIN ;")
        assertBool "" (isRight eRes)
    , testCase "test parse commit" $ do
        let eRes = runParser commit "" (T.toLower "COMMIT   ;")
        assertBool "" (isRight eRes)
    , testCase "test parse rollback" $ do
        let eRes = runParser rollback "" (T.toLower "Rollback   ;")
        assertBool "" (isRight eRes)
    ]

ignoreStatementTests :: TestTree
ignoreStatementTests =
  testGroup
    "Ignoring statement test"
    [ testBeginCommit
    ]

testCreateOrReplaceTable :: TestTree
testCreateOrReplaceTable =
  testCase "test create table if not exists statement" $ do
    let eRes = runParser parseCreateTable "" (T.toLower "create table if not exists x (y int)")
    case eRes of
      Left _ -> assertFailure "parsing failed for create if not exist"
      Right r -> simpleTable0 @=? r

testColumnConstraint :: TestTree
testColumnConstraint =
  testGroup
    "column constraints"
    [ testCase "column with constraint name" $
        testParse
          ( T.toLower
              "create \
              \table if not exists x (y varchar(3) CONSTRAINT \
              \cName primary key, z int)"
          )
          parseCreateTable
          Ex.constraintNamePK
          "parsing failed for column constraint"
    , testCase "column with Null and Not Null constraints" $
        testParse
          ( T.toLower
              "create \
              \table if not exists x (y varchar(3) Not NULL\
              \, z int NULL)"
          )
          parseCreateTable
          Ex.columnNullNotNull
          "parsing failed for column NULL, not NULL"
    , testCase "column with unique constraint" $
        testParse
          ( T.toLower
              "create \
              \table if not exists x (y varchar(3) unique\
              \, z int\
              \, constraint zUnique unique (z))"
          )
          parseCreateTable
          Ex.uniqueConstraint
          "parsing failed for unique column constraint"
    ]

{-
"create table if not exists x (y varchar(3) unique\
              \, z int, constraint zunique unique (z),\
              \constraint asd primary key (y), constraint fKey foreign key (z) references\
              \sometable (z) on update cascade, check (z > 23),\
              \constraint uniquex unique (y))"

-}

testTableConstraint :: TestTree
testTableConstraint =
  testGroup
    "table constraints"
    [ testCase "table constraints" $
        testParse
          ( T.toLower
              "create table if not exists x (y varchar(3) unique\
              \, z int, constraint zunique unique (z),\
              \constraint asd primary key (y), constraint fKey foreign key (z) references\
              \ sometable (z) on update cascade on delete cascade, check (z > 23),\
              \constraint uniquex unique (y,z))"
          )
          parseCreateTable
          Ex.tableConstraint
          "Parsing failed for table constraints"
    ]

testPartitionConstraint :: TestTree
testPartitionConstraint =
  testGroup
    "partition constraint"
    [ testCase "paition with multiple parenthesis" $
        testParse
          ( T.toLower
              "CREATE TABLE cities (\
              \city_id      int not null,\
              \name         text not null,\
              \population   bigint\
              \) PARTITION BY LIST (left(lower(name), 1))"
          )
          parseCreateTable
          Ex.partitionConstraint
          "Parsing failed for table with partition"
    ]

foreignKeyStatements :: TestTree
foreignKeyStatements =
  testGroup
    "Foreign key statements"
    [ testCase "paition with multiple parenthesis" $
        testParse
          ( T.toLower
              "CREATE TABLE summaries (\
              \summary_id SERIAL PRIMARY KEY,\
              \thread_id INTEGER NOT NULL REFERENCES threads (thread_id),\
              \summary_content TEXT NOT NULL,\
              \summary_created_at TIMESTAMP DEFAULT NOW(),\
              \summary_modified_at TIMESTAMP DEFAULT NOW())"
          )
          parseCreateTable
          Ex.foreginKeyStatement
          "Parsing failed for table with partition"
    ]

createStatementTests :: TestTree
createStatementTests =
  testGroup
    "Create statement tests"
    [ testCreateOrReplaceTable
    , testColumnConstraint
    , testTableConstraint
    , testPartitionConstraint
    , foreignKeyStatements
    ]

mermaidColumnReferenceTable :: TestTree
mermaidColumnReferenceTable =
  testGroup
    "Column reference table"
    [ testCase "column reference table 1" $ do
        "erDiagram\nsummaries {\n    serial summary_id\n    integer thread_id\n    text summary_content\n    timestamp summary_created_at\n    timestamp summary_modified_at\n}\n\nsummaries ||--o{ threads : thread_id\n\n\n\n\n\n"
          @=? generateMermaidERD [foreginKeyStatement]
    ]

mermaidDiagramTest :: TestTree
mermaidDiagramTest =
  testGroup
    "Mermaid diagram tests"
    [ mermaidColumnReferenceTable
    ]

testSqlScripts :: Text -> TestTree
testSqlScripts t = do
  let eRes = runParser parseSqlScript "" t
  testCase "parsing script" $ do
    assertBool "" (isRight eRes)

main :: IO ()
main = do
  testScriptContent <- T.toLower <$> T.readFile "./example/test.sql"
  defaultMain $
    testGroup
      "Tests"
      [ testSqlScripts testScriptContent
      , ignoreStatementTests
      , createStatementTests
      , mermaidDiagramTest
      ]
