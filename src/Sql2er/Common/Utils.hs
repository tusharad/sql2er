{-# LANGUAGE OverloadedStrings #-}

module Sql2er.Common.Utils
  ( lexeme
  , ignoreStatement
  , begin
  , commit
  , rollback
  ) where

import Control.Monad ( void )
import Sql2er.Common.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Text (Text)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") empty

begin :: Parser ()
begin = void $ lexeme (string "begin")

commit :: Parser ()
commit = void $ lexeme (string "commit")

rollback :: Parser ()
rollback = void $ lexeme (string "rollback")

ignoreStatement :: Parser ()
ignoreStatement = do
  choice $
    try
      <$> [ insertStatement
          , parseCreateFunction
          , createTrigger
          , createIndex
          , begin
          , commit
          , rollback
          ]
  return ()
  where
    insertStatement = void $ lexeme (string "insert") *> takeWhile1P Nothing (/= ';')
    -- createFunction = do
    --   _ <- lexeme (string "create")
    --   _ <- optional $ lexeme (string "or") *> lexeme (string "replace")
    --   _ <- lexeme (string "function") *> takeRest
    --   return ()
    createTrigger = do
      _ <- lexeme (string "create")
      _ <- optional $ lexeme (string "or") *> lexeme (string "replace")
      _ <- lexeme (string "trigger")
      _ <- optional $ lexeme (string "or") *> lexeme (string "replace")
      void $ lexeme (takeWhile1P Nothing (/= ';'))
    createIndex = do
      void $ lexeme (string "create")
      void $ optional (lexeme (string "unique"))
      void $ lexeme (string "index")
      void $ lexeme (takeWhileP Nothing (/= ';'))

keyword :: Text -> Parser Text
keyword kw = lexeme (string kw)

-- Parser for function name and arguments
parseName :: Parser Text
parseName = lexeme (takeWhile1P Nothing (`notElem` (" \t\n,();" :: String)))

parseArgs :: Parser [Text]
parseArgs = between (lexeme (char '(')) (lexeme (char ')')) (parseName `sepBy` char ',')

parseReturnType :: Parser (Maybe Text)
parseReturnType = optional $ lexeme (string "returns") *> parseName

parseFunctionBody :: Parser Text
parseFunctionBody = do 
  void $ optional (lexeme $ string "as")
  between (lexeme (string "$$")) (lexeme (string "$$")) (takeWhileP Nothing (/= '$'))

parseLanguage :: Parser Text
parseLanguage = keyword "language" *> parseName

parseCreateFunction :: Parser ()
parseCreateFunction = do
  void $ lexeme $ string "create"
  void $ optional (lexeme (string "or") *> lexeme (string "replace"))
  void $ lexeme (string "function")
  void parseName
  void parseArgs
  void parseReturnType
  void parseFunctionBody
  void parseLanguage
