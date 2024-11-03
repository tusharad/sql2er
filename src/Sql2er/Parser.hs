module Sql2er.Parser where

import Sql2er.Common.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Sql2er.Parser.CreateTable (parseCreateTable)
import Sql2er.Parser.AlterTable (parseAlterTable)
import Sql2er.Parser.DropTable (parseDropTable)
import Sql2er.Common.Utils (lexeme, ignoreStatement)
import Sql2er.Parser.CreateType (parseCreateType)
import Data.Maybe

parseSql :: Parser Statement
parseSql = choice [
        try $ CreateTypeStatement <$> parseCreateType
      , try $ CreateStatement <$> parseCreateTable
      , try $ AlterStatement <$> parseAlterTable
      , try $ DropStatement <$> parseDropTable
    , EOF <$ eof 
    ]

parseSqlScript :: Parser [Statement]
parseSqlScript = do 
    res <- (Nothing <$ ignoreStatement <|> Just <$> parseSql) `sepBy` lexeme (char ';')
    return $ catMaybes res


