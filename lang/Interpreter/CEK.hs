{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

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

data InterpreterState
  = InterpreterState
  { _control :: Term
  , _environment :: Environment
  , _kontinuation :: Kont
  , _typeAssignments :: TI.Subst
  , _tiCounter ::Int
  }
  deriving (Eq, Show)

makeLenses ''InterpreterState


type MInterpret = StateT InterpreterState (Except T.Text)

initialState t = InterpreterState
  { _control = t
  , _environment = emptyEnvironment
  , _kontinuation = Terminate
  , _typeAssignments = TI.nullSubst
  , _tiCounter = 0
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
         Just (Closure v b e) -> do
           control <.= Lam v b
           let ctx = (curr ^. environment) & set bindings e
           environment <.= ctx
           interpret
         Just v -> return v
         Nothing -> throwError $ "unbound variable: " <> n
    (App f x) -> do
       control <.= f
       kontinuation <.= Argument x (curr ^. environment) (curr ^. kontinuation)
       interpret

    (Lam arg term) -> case curr ^. kontinuation of
      Terminate -> return $ Closure arg term (curr ^. environment ^. bindings)
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

    (Rec rows) -> case curr ^. kontinuation of
        Terminate -> VTable <$> mapM interpretInner rows
        Function (Lam v b) env k  -> do
          t <- mapM interpretInner rows
          control <.= b
          environment <.= extend v (VTable t) env
          kontinuation <.= k
          interpret
        otherwise -> throwError $ "not a function: " <> (T.pack . show $ rows)
 where
   interpretInner (label, term) = do
     control <.= term
     v <- interpret
     return (label, v)

   newTyVar :: T.Text -> MInterpret Ty
   newTyVar prefix = do
     c <- tiCounter <+= 1
     return (TVar (prefix <> (T.pack . show $ c)))
     
   instantiate :: TI.TypeScheme -> MInterpret Ty
   instantiate (TI.TypeScheme vars t) = do
     nvars <- mapM (\_ -> newTyVar "a") vars
     let s = Map.fromList (zip vars nvars)
     return $ TI.apply s t

         

run :: MInterpret a -> InterpreterState -> Either T.Text (a, InterpreterState)
run s = (runExcept . runStateT s)
