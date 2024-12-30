{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Sql2er.Parser (parseSqlScript, postParsingSetup)
import System.Exit (exitFailure)
import Text.Megaparsec
import Sql2er.CmdArgs (Options(..), parseCmdArgs)
import Sql2er.Mermaid (generateMermaidERD)
import Data.Text.Encoding.Base64.URL

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
      print $ generateMermaidERD tableList
      print $ encodeBase64 $ generateMermaidERD tableList