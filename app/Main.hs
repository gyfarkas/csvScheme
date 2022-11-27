{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T

import Interpreter.CEK
import Interpreter.TypeCheck
import Data.Types.Ty
import Data.Value.Value
import Data.Term.Term
import qualified Data.Map as Map
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
            let types = typeInference form
            case types of
              Left e -> do
                  outputStrLn . show $ e
              Right (t, s) -> do
                  outputStrLn . T.unpack . pp $ t
                  case run interpret (initialState form) of
                    Left e -> outputStrLn . show $ e
                    Right (v, s) -> outputStrLn . T.unpack . ppValue $ v
            repl
