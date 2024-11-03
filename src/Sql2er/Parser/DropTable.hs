{-# LANGUAGE OverloadedStrings #-}

module Sql2er.Parser.DropTable where

import Sql2er.Common.Types
import Sql2er.Common.Utils
import Sql2er.Parser.Common
import Text.Megaparsec
import Text.Megaparsec.Char

parseDropTable :: Parser DropTable
parseDropTable = do
  _ <- lexeme (string "drop") *> lexeme (string "table")
  _ <- optional $ lexeme (string "if") *> lexeme (string "exists")
  tables <- parseWordForAlter `sepBy1` lexeme (char ',') -- Allow comma-separated list of table names
  option_ <- optional (parseCascade <|> parseRestrict)
  return $ DropTable tables option_

parseCascade :: Parser DropTableOption
parseCascade = Cascade <$ lexeme (string "cascade")

parseRestrict :: Parser DropTableOption
parseRestrict = Restrict <$ lexeme (string "restrict")
