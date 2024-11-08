{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Sql2er.Parser
import Sql2er.Common.Utils (begin, commit, rollback)
import Data.Text.IO qualified as T
import Data.Text qualified as T
import Data.Text (Text)
import Data.Either
import Text.Megaparsec

testBeginCommit :: TestTree
testBeginCommit = testGroup "Begin commit" [
    testCase "test parse begin" $ do
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
ignoreStatementTests = testGroup "Ignoring statement test" [
    testBeginCommit
    ]

testSqlScripts :: Text -> TestTree
testSqlScripts t = do
    let eRes = runParser parseSqlScript "" t
    testCase "parsing script" (assertBool "" (isRight eRes))

main :: IO ()
main = do
    testScriptContent <- T.toLower <$> T.readFile "./example/test.sql"
    defaultMain $ testGroup "Tests" [
        testSqlScripts testScriptContent
      , ignoreStatementTests ]
