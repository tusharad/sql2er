{-# LANGUAGE OverloadedStrings #-}

module Sql2er.Common.Utils
  ( lexeme
  , ignoreStatement
  , begin
  , commit
  , rollback
  ) where

import Control.Monad
import Sql2er.Common.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

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
          , createFunction
          , createTrigger
          , createIndex
          , begin
          , commit
          , rollback
          ]
  return ()
  where
    insertStatement = void $ lexeme (string "insert") *> takeWhile1P Nothing (/= ';')
    createFunction = do
      _ <- lexeme (string "create")
      _ <- optional $ lexeme (string "or") *> lexeme (string "replace")
      _ <- lexeme (string "function") *> takeRest
      return ()
    createTrigger = do
      _ <- lexeme (string "create") *> lexeme (string "trigger")
      _ <- optional $ lexeme (string "or") *> lexeme (string "replace")
      void $ lexeme (takeWhile1P Nothing (/= ';'))
    createIndex = do
      void $ lexeme (string "create")
      void $ optional (lexeme (string "unique"))
      void $ lexeme (string "index")
      void $ lexeme (takeWhileP Nothing (/= ';'))

-- void $ manyTill anySingle (char ';')
