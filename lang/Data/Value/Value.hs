{-# LANGUAGE TemplateHaskell #-}
module Data.Value.Value where

import Control.Lens.TH
import qualified Data.Text as T
import qualified Data.Map as Map

import Data.Term.Term

data Value
  = VInt Int
  | VText T.Text
  | VBool Bool
  | VTable [(T.Text, Value)]
  | Closure T.Text Term (Map.Map T.Text Value)
  deriving (Eq, Show)

makePrisms ''Value
