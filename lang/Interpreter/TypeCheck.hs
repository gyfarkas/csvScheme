{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.TypeCheck where
import Control.Lens.TH
import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set ((\\))
import Data.Types.Ty
import Data.Term.Term

-- for representing generic types
data TypeScheme =
  TypeScheme {
      _free :: [T.Text]
    , _ty :: Ty }
makeLenses ''TypeScheme 

-- substitution variable to type
type Subst = Map.Map T.Text Ty

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

-- type operations
class Typeable a where
    freeVars :: a -> Set.Set T.Text
    apply :: Subst -> a -> a

instance Typeable Ty where
    freeVars (TVar a) = Set.singleton a
    freeVars (TInt) = Set.empty
    freeVars (TString)  = Set.empty
    freeVars (TBool) = Set.empty
    freeVars (TFn t1 t2) = freeVars t1 `Set.union` freeVars t2
    freeVars (TRecord _ fieldTypes) = foldr (Set.union . freeVars . snd) Set.empty fieldTypes
    apply subst (TFn a b) = TFn (apply subst a) (apply subst b)
    apply subst t = t

instance Typeable TypeScheme where
    freeVars ts = freeVars (ts ^. ty) \\ (Set.fromList (ts ^. free))
    apply subst ts = ts & ty %~ apply (foldr Map.delete subst (ts ^. free))

instance Typeable a => Typeable [a] where
    freeVars ts = foldr (Set.union . freeVars) Set.empty ts
    apply subst = fmap (apply subst)

newtype TypeEnv = TypeEnv (Map.Map String TypeScheme)



