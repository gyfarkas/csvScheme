{-# LANGUAGE OverloadedStrings #-}
module Parser where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Char
import Control.Monad.Identity

import Data.Term.Term

type TermParser = Parsec [Char] Term

sourceName1 :: SourceName
sourceName1 = "input"

parse :: T.Text -> Either ParseError Term
parse s = runParser form () sourceName1 (T.unpack s)
  where
      boolean = do
        st <- string "true" <|> string "false"
        return $ B (st == "true")
      stringLiteral = do
          string "\""
          word <- T.pack <$> many1 (noneOf "\"")
          string "\""
          return $ S word
      int = I . read <$> many1 digit
      labelledValuePair = do
          label <- T.pack <$> many (noneOf ":}")
          spaces
          string ":"
          spaces
          value <- form
          return (Label label, value)
      tableLiteral = Rec <$> (between (char '{') (char '}') $ labelledValuePair `sepBy` (char ','))
      form = boolean
          <|> int
          <|> stringLiteral
          <|> tableLiteral
          <|> closed
          <|> variable
      variable = Var . T.pack <$> many1 letter
      fun = do
          string "lambda"
          spaces
          varName <- T.pack <$> (many1 letter)
          spaces
          body <- form
          return $ Lam varName body
      app = do
          x <- form
          spaces
          y <- form
          return $ App x y
      closed = do
          char '('
          spaces
          x <- fun <|> app
          spaces
          char ')'
          return x
