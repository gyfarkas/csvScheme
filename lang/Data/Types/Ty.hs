{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Types.Ty where

import Control.Lens.TH
import qualified Data.Text as T
import Data.Term.Term

typeToList :: Ty -> ([(Label, Ty)], Maybe T.Text)
typeToList (TVar r)           = ([], Just r)
typeToList (TRecord (ExtendRow (l, t) r)) =
  let (ls, mv) = typeToList r in ((l, t):ls, mv)
typeToList _ = ([], Nothing)

-- Types of the language
data Ty
 = TInt        -- Integer type
 | TString     -- String type
 | TBool       -- boolean type
 | TVar T.Text -- type variables a1...
 | TFn Ty Ty   -- function type a -> b
 | TRecord Ty  -- anonymous records like {a:1, b:"s", c:true}
 | EmptyRow    -- unit
 | ExtendRow (Label, Ty) Ty -- row cons
 deriving (Eq, Show)

makePrisms ''Ty

pp :: Ty -> T.Text
pp TInt = "Int"
pp TString = "String"
pp TBool = "Boolean"
pp (TVar name) = name
pp (TFn a b) = "(" <> (pp a) <> " -> "  <> (pp b) <> ")"
pp EmptyRow = ""
pp (ExtendRow (l, t) r) = (unLabel l) <> ":" <> (pp t) <> "," <> (pp r) 
pp (TRecord ts) = "[" <> (T.intercalate "," $ ppRow ts) <> "]"
  where
    ppRow (ExtendRow (l, t) r) = (unLabel l) <> ":" <> (pp t) : (ppRow r)
    ppRow (TVar n) = [n]
    ppRow _ = []
