{-# LANGUAGE TemplateHaskell #-}
module Data.Term.Term where

import qualified Data.Text as T
import Control.Lens.TH

newtype Label = Label {unLabel :: T.Text} deriving (Eq, Show)

data Term
  = I Int
  | B Bool
  | S T.Text
  | Var T.Text
  | Lam T.Text Term
  | App Term Term
  | Rec [(Label, Term)]
  deriving (Eq, Show)

makePrisms ''Term
