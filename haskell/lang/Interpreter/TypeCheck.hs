{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
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
      _vars :: [T.Text]
    , _ty :: Ty }
makeLenses ''TypeScheme

-- substitution variable to type
type Subst = Map.Map T.Text Ty

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- type operations
class Typeable a where
    freeVars :: a -> Set.Set T.Text
    apply :: Subst -> a -> a

instance Typeable Ty where
    freeVars (TVar a) = Set.singleton a
    freeVars TInt = Set.empty
    freeVars TString  = Set.empty
    freeVars TBool = Set.empty
    freeVars (TFn t1 t2) = freeVars t1 `Set.union` freeVars t2
    freeVars (TRecord row) = freeVars row
    freeVars EmptyRow = Set.empty
    freeVars (ExtendRow (l, t) r) = freeVars t `Set.union` freeVars r
    apply subst (TFn a b) = TFn (apply subst a) (apply subst b)
    apply s (TRecord t) = TRecord (apply s t)
    apply s (ExtendRow (l, t) r) = ExtendRow (l, apply s t) (apply s r)
    apply subst (TVar a) = case Map.lookup a subst of
       Nothing -> TVar a
       Just t -> t
    apply subst t = t

instance Typeable TypeScheme where
    freeVars ts = freeVars (ts ^. ty) \\ Set.fromList (ts ^. vars)
    apply subst ts = ts & ty %~ apply (foldr Map.delete subst (ts ^. vars))

instance Typeable a => Typeable [a] where
    freeVars = foldr (Set.union . freeVars) Set.empty
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
-- records are unifieable
-- when forall label in r1 is in r2 and have unifieable type.
unify (TRecord row1) (TRecord row2) = unify row1 row2
unify EmptyRow EmptyRow = return nullSubst
unify (ExtendRow (label1, fieldTy1) rowTail1) row2@ExtendRow{} = do
  (fieldTy2, rowTail2, theta1) <- rewriteRow row2 label1
  -- ^ apply side-condition to ensure termination
  case snd $ typeToList rowTail1 of
    Just tv | Map.member tv theta1 -> throwError "recursive row type"
    _ -> do
      theta2 <- unify (apply theta1 fieldTy1) (apply theta1 fieldTy2)
      let s = theta2 `composeSubst` theta1
      theta3 <- unify (apply s rowTail1) (apply s rowTail2)
      return $ theta3 `composeSubst` s
unify t1 t2 =
    throwError $ "types do not unify: "
      <> (T.pack . show $ t1)
      <> " vs. "
      <> (T.pack . show $ t2)


rewriteRow :: Ty -> Label -> MTypecheck (Ty, Ty, Subst)
rewriteRow EmptyRow newLabel = throwError $ "label " <> unLabel newLabel <> " cannot be inserted"
rewriteRow (ExtendRow (label, fieldTy) rowTail) newLabel
  | newLabel == label = return (fieldTy, rowTail, nullSubst) -- ^ nothing to do
  | TVar alpha <- rowTail = do
      beta  <- newTyVar "r"
      gamma <- newTyVar "a"
      return ( gamma
             , ExtendRow (label, fieldTy) beta
             , Map.singleton alpha $ ExtendRow (newLabel, gamma) beta
             )
  | otherwise = do
      (fieldTy', rowTail', s) <- rewriteRow rowTail newLabel
      return ( fieldTy'
             , ExtendRow (label, fieldTy) rowTail'
             , s
             )
rewriteRow ty _ = throwError $ "Unexpected type: " <> (T.pack . show $ ty)

varBind :: T.Text -> Ty -> MTypecheck Subst
varBind u t
  | t == TVar u = return nullSubst
  | u `Set.member` freeVars t =
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
    let r = TRecord . listToRow $ typeList
    return (subst, r)
  where
    listToRow :: [(Label, Ty)] -> Ty
    listToRow = foldr ExtendRow EmptyRow
   
    folding :: (Subst, [(Label, Ty)]) -> [(Label, Term)] -> MTypecheck (Subst, [(Label, Ty)])  =
      foldM $ \(subst, ts) (l, term) -> do
        (subst', t') <- ti env term
        let subst'' = composeSubst subst subst'
        let labelledTypes = ts ++ [(l, t')]
        return (subst'', labelledTypes)
ti env (Lam n e) = do
    tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `Map.union` Map.singleton n (TypeScheme [] tv))
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
ti env (BuiltIn (Plus i1 i2)) =
    return (nullSubst, TFn TInt (TFn TInt TInt))
ti env (BuiltIn (Project label r)) = do
    a <- newTyVar "a"
    rv <- newTyVar "r"
    return (nullSubst, TFn (TRecord $ ExtendRow (label, a) rv) a)
ti env (BuiltIn (Extend (label, v) r)) = do
    a <- newTyVar "a"
    rv <- newTyVar "r"
    return (nullSubst, TFn a (TFn (TRecord rv) (TRecord $ ExtendRow (label, a) rv)))
ti env (BuiltIn (Remove label r)) = do
    a <- newTyVar "a"
    rv <- newTyVar "r"
    return (nullSubst, TFn (TRecord $ ExtendRow (label, a) rv) (TRecord rv))
findLabel :: Label -> Ty -> Maybe Ty
findLabel l (TRecord row) = findLabel l row
findLabel l (ExtendRow (l', t) r)
  | l' == l = Just t
  | otherwise = findLabel l r
findLabel l t = Nothing

runTypecheck :: TypeState -> (MTypecheck a ->  Either T.Text (a, TypeState))
runTypecheck s = runExcept . flip runStateT s

typeInference :: Term -> Either T.Text (Ty, TypeState)
typeInference e = runTypecheck emptyTypeState $ do
    (s,t) <- ti (TypeEnv Map.empty) e
    return (apply s t)
