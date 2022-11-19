{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter.TypeCheck where
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
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
    apply subst (TRecord n ts) = TRecord n $ fmap (\(l, t) -> (l, ) . (apply subst) $ t) ts
    apply subst (TVar a) = case Map.lookup a subst of
       Nothing -> TVar a
       Just t -> t
    apply subst t = t

instance Typeable TypeScheme where
    freeVars ts = freeVars (ts ^. ty) \\ (Set.fromList (ts ^. free))
    apply subst ts = ts & ty %~ apply (foldr Map.delete subst (ts ^. free))

instance Typeable a => Typeable [a] where
    freeVars ts = foldr (Set.union . freeVars) Set.empty ts
    apply subst = fmap (apply subst)

newtype TypeEnv = TypeEnv (Map.Map T.Text TypeScheme)

instance Typeable TypeEnv where
    freeVars (TypeEnv env) = freeVars (Map.elems env)
    apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

data TypeState = TypeState {
    _typeAssignments :: Subst
  , _tiCounter ::Int
} deriving (Eq, Show)

emptyTypeState = TypeState { _typeAssignments = nullSubst , _tiCounter = 0 }

makeLenses ''TypeState

type MTypecheck = StateT TypeState (Except T.Text)

newTyVar :: T.Text -> MTypecheck Ty
newTyVar prefix = do
  c <- tiCounter <+= 1
  return (TVar (prefix <> (T.pack . show $ c)))

instantiate :: TypeScheme -> MTypecheck Ty
instantiate (TypeScheme vars t) = do
  nvars <- mapM (\_ -> newTyVar "a") vars
  let s = Map.fromList (zip vars nvars)
  return $ apply s t

remove :: TypeEnv -> T.Text -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

unify:: Ty -> Ty -> MTypecheck Subst
unify (TFn l r)(TFn l' r') = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return (s1 `composeSubst` s2)
unify (TVar u) t = varBind u t
unify t (TVar u) = varBind u t
unify TInt TInt = return nullSubst
unify TBool TBool = return nullSubst
unify (TRecord n ts) (TRecord n2 ts') =
    if n == n2
    then folding nullSubst $ zip (fmap snd ts) (fmap snd ts')
    else throwError $ "record " <> n <> " and record " <> n2 <> " do not unify"
  where folding =  foldM $ \subst (t1, t2) -> do
          subst' <- unify t1 t2
          return $ composeSubst subst subst'
unify t1 t2 =
    throwError $ "types do not unify: "
      <> (T.pack . show $ t1)
      <> " vs. "
      <> (T.pack . show $ t2)

varBind :: T.Text -> Ty -> MTypecheck Subst
varBind u t
  | t == (TVar u) = return nullSubst
  | u `Set.member` (freeVars t) =
      throwError $ "occur check fails: " <> u <> " vs. " <> (T.pack . show $ t)
  | otherwise = return (Map.singleton u t)

ti :: TypeEnv -> Term ->  MTypecheck (Subst , Ty )
ti (TypeEnv env) (Var n) =
    case Map.lookup n env of
      Nothing -> throwError $ "unbound variable: " <> n
      Just sigma -> do
          t <- instantiate sigma
          return (nullSubst, t)
ti env (S l) = pure (nullSubst, TString)
ti env (I l) = pure (nullSubst, TInt)
ti env (B l) = pure (nullSubst, TBool)
ti env (Rec ts) = do
    (subst, typeList) <- folding (nullSubst, []) ts
    let r = TRecord "anon_placeholder" typeList
    return (subst, r)
  where
    folding :: (Subst, [(Label, Ty)]) -> [(Label, Term)] -> MTypecheck (Subst, [(Label, Ty)])  =
      foldM $ \(subst, ts) (l, term) -> do
        (subst', t') <- ti env term
        let subst'' = composeSubst subst subst'
        let labelledTypes = ts ++ [(l, t')]
        return (subst'', labelledTypes)
ti env (Lam n e) = do
    tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `Map.union` (Map.singleton n (TypeScheme [] tv)))
    (s1,t1) <- ti env'' e
    return (s1, TFn (apply s1 tv ) t1 )
ti env (App e1 e2) = do
    tv <- newTyVar "a"
    (s1,t1) <- ti env e1
    (s2,t2) <- ti (apply s1 env) e2
    s3 <- unify (TFn t2 tv) (apply s2 t1)
    let f = s3 `composeSubst` s2 `composeSubst` s1
    typeAssignments <.= f
    return (f, apply s3 tv)
ti env (BuiltIn (Plus i1 i2)) = do
    (s1, t1) <- ti env i1
    (s2, t2) <- ti (apply s1 env) i2
    s1' <- unify TInt t1
    s2' <- unify TInt t2
    return (s1' `composeSubst` s2', TInt)
ti env (BuiltIn _) = throwError "unimplemented"

runTypecheck :: TypeState -> ((MTypecheck a) ->  Either T.Text (a, TypeState))
runTypecheck s = (runExcept . (flip runStateT s))

typeInference :: Term -> Either T.Text (Ty, TypeState)
typeInference e = runTypecheck emptyTypeState $ do
    (s,t) <- ti (TypeEnv Map.empty) e
    return (apply s t)
