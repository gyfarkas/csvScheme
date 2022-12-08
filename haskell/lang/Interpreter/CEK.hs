{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Interpreter.CEK where

import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Fail
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Lens
import Control.Lens.Operators
import Control.Lens.TH


import Data.Types.Ty
import Data.Value.Value
import Data.Term.Term
import qualified Interpreter.TypeCheck as TI

newtype Table a = Table [[a]]

data Environment
  = Environment
    { _bindings :: Map.Map T.Text Value
    }
    deriving (Eq, Show)

makeLenses ''Environment

emptyEnvironment = Environment { _bindings = Map.empty }

extend :: T.Text -> Value -> Environment -> Environment
extend v b ctx = ctx & bindings . (at v) ?~ b

data Kont
  = Terminate
  | Argument Term Environment Kont
  | Function Term Environment Kont
  deriving (Eq, Show)

{--
   (. a (. b {b:{a:1}})) e k ->
   -> (. b {b: {a:1}}) e (Project a k)
   -> {b: {a:1}} e (Project b e (Project a e k))
  -> (lookup b {b: {a:1}})={a:1} (Project a e k)
  -> (lookup a {a:1}=1) e k
-}

data InterpreterState
  = InterpreterState
  { _control :: Term
  , _environment :: Environment
  , _kontinuation :: Kont
  }
  deriving (Eq, Show)

makeLenses ''InterpreterState


type MInterpret = StateT InterpreterState (Except T.Text)

initialState t = InterpreterState
  { _control = t
  , _environment = emptyEnvironment
  , _kontinuation = Terminate
  }


interpret :: MInterpret Value
interpret = do
  curr <- get
  case (curr ^. control) of
    (I int) ->
      case curr ^. kontinuation of
        Terminate -> return $ VInt int
        Function (Lam v b) env k  -> do
          control <.= b
          environment <.= extend v (VInt int) env
          kontinuation <.= k
          interpret
        otherwise -> throwError $ "not a function: " <> (T.pack . show $ int)
    (B bool) ->
      case curr ^. kontinuation of
        Terminate -> return $ VBool bool
        Function (Lam v b) env k  -> do
          control <.= b
          environment <.= extend v (VBool bool) env
          kontinuation <.= k
          interpret
        otherwise -> throwError $ "not a function: " <> (T.pack . show $ bool)
    (S t) ->
      case curr ^. kontinuation of
        Terminate -> return $ VText t
        Function (Lam v b) env k  -> do
          control <.= b
          environment <.= extend v (VText t) env
          kontinuation <.= k
          interpret
        otherwise -> throwError $ "not a function: " <> (T.pack . show $ t)
    (Var n) -> do
       case (Map.lookup n (curr ^. environment ^. bindings)) of
         Just v -> return v
         Nothing -> throwError $ "unbound variable: " <> n

    (App f x) -> do
       control <.= f
       kontinuation <.= Argument x (curr ^. environment) (curr ^. kontinuation)
       interpret

    (Lam arg term) -> case curr ^. kontinuation of
      Argument term' ctx' k' -> do
        control <.= term'
        kontinuation <.= Function (Lam arg term) (curr ^. environment) k'
        environment <.= ctx'
        interpret
      Function (Lam arg' term') ctx' k' -> do
        let e = curr ^. environment ^. bindings
        control <.= term'
        kontinuation <.= k'
        environment <.= extend arg' (Closure arg term e) ctx'
        interpret
      Terminate -> return $ Closure arg term (curr ^. environment ^. bindings)
    (Rec row) -> case curr ^. kontinuation of
        Terminate -> VRow <$> mapM (interpretInner $ curr ^. environment) row
        Function (Lam v b) env k  -> do
          t <- mapM (interpretInner env) row
          control <.= b
          environment <.= extend v (VRow t) env
          kontinuation <.= k
          interpret
        otherwise -> throwError $ "not a function: " <> (T.pack . show $ row)
    BuiltIn b -> builtInEval b

  where
   interpretInner :: Environment -> (Label, Term) -> MInterpret (Label, Value)
   interpretInner env (label, term) = do
     curr <- get
     v <- evalToValue term env
     return (label, v)

   valToTerm :: Value -> Term
   valToTerm = \case
     (Closure v b e) ->
       Lam v b -- danger should save the environment somehow
     (VInt v) -> I v
     (VText v) -> S v
     (VBool b) -> B b
     (VRow values) ->
       Rec $ (\(l, v) -> (l, valToTerm v)) <$> values
   evalToValue :: Term -> Environment -> MInterpret Value
   evalToValue e env = do
     control <.= e
     environment <.= env
     kontinuation <.= Terminate
     interpret

   builtInEval :: BuiltInFn -> MInterpret Value
   builtInEval b = case b of
     Plus l r -> do
       curr <- get
       lv <- evalToValue l (curr ^. environment)
       rv <- evalToValue r (curr ^. environment)
       case (lv, rv) of
         (VInt x, VInt y) -> return $ VInt (x + y)
         _ -> throwError "invalid plus arguments"
     Extend (l,v) r -> do
       curr <- get
       rc <- evalToValue r (curr ^. environment)
       vv <- evalToValue v (curr ^. environment)
       case rc of
         (VRow fields) -> return . VRow $ (l,vv):fields
         x -> throwError $ "invalid extension base" <> (T.pack . show $ x)
     Remove l r -> do
       curr <- get
       rc <- evalToValue r (curr ^.environment)
       case rc of
         (VRow fields) -> return . VRow $ filter ((/= l) . fst) fields
         x -> throwError $ "invalid removal base" <> (T.pack . show $ x)
     Project l r -> do
       curr <- get
       rc <- evalToValue r (curr ^. environment)
       case rc of
         (VRow fields) -> do
           v <-  maybe (throwError "invalid label") return (lookup l fields)
           return $ v
         x -> throwError $ "invalid projection base" <> (T.pack . show $ x)

run :: MInterpret a -> InterpreterState -> Either T.Text (a, InterpreterState)
run s = (runExcept . runStateT s)
