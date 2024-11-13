module Sql2er.Parser (parseSql, parseSqlScript) where

import Data.Maybe
import Sql2er.Common.Types
import Sql2er.Common.Utils (ignoreStatement, lexeme)
import Sql2er.Parser.AlterTable (parseAlterTable)
import Sql2er.Parser.CreateTable (parseCreateTable)
import Sql2er.Parser.CreateType (parseCreateType)
import Sql2er.Parser.DropTable (parseDropTable)
import Text.Megaparsec
import Text.Megaparsec.Char

parseSql :: Parser Statement
parseSql =
  choice
    [ try $ CreateTypeStatement <$> parseCreateType
    , try $ CreateStatement <$> parseCreateTable
    , try $ AlterStatement <$> parseAlterTable
    , try $ DropStatement <$> parseDropTable
    , EOF <$ eof
    ]

parseSqlScript :: Parser [Statement]
parseSqlScript = do
  res <- (Nothing <$ ignoreStatement <|> Just <$> parseSql) `sepBy` lexeme (char ';')
  return $ catMaybes res
