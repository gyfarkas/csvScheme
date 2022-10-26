{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Text as T

import Interpreter.CEK
import Data.Types.Ty
import Data.Value.Value
import Data.Term.Term
import Parser
import System.Console.Haskeline

 
main :: IO ()
main = runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  maybeLine <- getInputLine "csvScheme> "
  case maybeLine of
    Nothing -> return ()
    Just ":quit" -> return ()
    Just text ->
      case  parse (T.pack text) of
        Left e -> do
           outputStrLn . show $ e
           repl
        Right form -> do
            outputStrLn . show $ form
            outputStrLn . show $ run interpret (initialState form)
            repl
