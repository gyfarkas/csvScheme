{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Types.Ty where

import Control.Lens.TH
import qualified Data.Text as T
import Data.Term.Term

data Ty
 = TInt
 | TString
 | TBool
 | TVar T.Text
 | TFn Ty Ty
 | TRecord T.Text [(Label, Ty)]
 deriving (Eq, Show)

makePrisms ''Ty


pp :: Ty -> T.Text
pp TInt = "Int"
pp TString = "String"
pp TBool = "Boolean"
pp (TVar name) = name
pp (TFn a b) = "(" <> (pp a) <> " -> "  <> (pp b) <> ")"
pp (TRecord n ts) =
    "[" <>
    (foldl (<>) "" $ fmap (\(l, t) -> ((unLabel l) <> ":" <> (pp t)))  ts) <>
    "]"
