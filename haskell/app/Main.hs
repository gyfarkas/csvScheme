{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Text.RawString.QQ
logo :: T.Text
logo = [r|
                     __      _
      ___ _____   __/ _\ ___| |__   ___ _ __ ___   ___
     / __/ __\ \ / /\ \ / __| '_ \ / _ \ '_ ` _ \ / _ \
    | (__\__ \\ V / _\ \ (__| | | |  __/ | | | | |  __/
     \___|___/ \_/  \__/\___|_| |_|\___|_| |_| |_|\___|  |]

main :: IO ()
main = do
    putStrLn . T.unpack $ logo

    runInputT defaultSettings repl

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
            outputStrLn (show form)
            let types = typeInference form
            case types of
              Left e -> do
                  outputStrLn . show $ e
              Right (t, s) -> do
                  outputStr " : "
                  outputStrLn . T.unpack . pp $ t
                  case run interpret (initialState form) of
                    Left e -> outputStrLn . show $ e
                    Right (v, s) -> outputStrLn . T.unpack . ppValue $ v
            repl
