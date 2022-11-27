{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Value.Value where

import Control.Lens.TH
import qualified Data.Text as T
import qualified Data.Map as Map

import Data.Term.Term

data Value
  = VInt Int
  | VText T.Text
  | VBool Bool
  | VRow [(Label, Value)]
  | Closure T.Text Term (Map.Map T.Text Value)
  deriving (Eq, Show)

makePrisms ''Value

ppValue = \case
  Closure _ _ _ -> "Closure"
  t -> T.pack . show $ t
