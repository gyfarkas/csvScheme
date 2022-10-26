{-# LANGUAGE TemplateHaskell #-}

module Data.Types.Ty where

import Control.Lens.TH
import qualified Data.Text as T

data Ty
 = TInt
 | TString
 | TFn Ty Ty
 | TRecord T.Text [Ty]
 | TUnion T.Text [(T.Text, Ty)]
 deriving (Eq, Show)

makePrisms ''Ty
