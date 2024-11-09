{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import ExamplesForTests
import ExamplesForTests qualified as Ex
import Sql2er.Common.Utils (begin, commit, rollback)
import Sql2er.Parser
import Sql2er.Parser.CreateTable (parseCreateTable)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

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
  testCase "test create or replace table statement" $ do
    let eRes = runParser parseCreateTable "" (T.toLower "create or replace table x (y int)")
    case eRes of
      Left _ -> assertFailure "parsing failed for create or replace"
      Right r -> simpleTable0 @=? r

testColumnConstraint :: TestTree
testColumnConstraint =
  testGroup
    "column constraints"
    [ testCase "column with constraint name" $ do
        let eRes =
              runParser
                parseCreateTable
                ""
                ( T.toLower
                    "create or replace \
                    \table x (y int CONSTRAINT \
                    \cName primary key, z int)"
                )
        case eRes of
          Left _ -> assertFailure "parsing failed for column constraint"
          Right r -> Ex.constraintNamePK @=? r
    ]

createStatementTests :: TestTree
createStatementTests =
  testGroup
    "Create statement tests"
    [ testCreateOrReplaceTable
    , testColumnConstraint
    ]

testSqlScripts :: Text -> TestTree
testSqlScripts t = do
  let eRes = runParser parseSqlScript "" t
  testCase "parsing script" (assertBool "" (isRight eRes))

main :: IO ()
main = do
  testScriptContent <- T.toLower <$> T.readFile "./example/test.sql"
  defaultMain $
    testGroup
      "Tests"
      [ testSqlScripts testScriptContent
      , ignoreStatementTests
      , createStatementTests
      ]
