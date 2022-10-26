{-# LANGUAGE TemplateHaskell #-}
module Data.Term.Term where

import qualified Data.Text as T
import Control.Lens.TH

data Term
  = I Int
  | S T.Text
  | Var T.Text
  | Lam T.Text Term
  | App Term Term
  | Rec [(T.Text, Term)]
  deriving (Eq, Show)

makePrisms ''Term
