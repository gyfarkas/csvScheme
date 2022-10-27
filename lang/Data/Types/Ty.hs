{-# LANGUAGE TemplateHaskell #-}

module Data.Types.Ty where

import Control.Lens.TH
import qualified Data.Text as T

data Ty
 = TInt
 | TString
 | TBool
 | TVar T.Text
 | TFn Ty Ty
 | TRecord T.Text [(T.Text, Ty)]
 deriving (Eq, Show)

makePrisms ''Ty
