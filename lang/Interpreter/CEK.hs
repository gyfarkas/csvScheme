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
         Just v -> interpretWithValue v
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
        Terminate -> VRow <$> mapM interpretInner rows
        Function (Lam v b) env k  -> do
          t <- mapM interpretInner rows
          control <.= b
          environment <.= extend v (VRow t) env
          kontinuation <.= k
          interpret
        otherwise -> throwError $ "not a function: " <> (T.pack . show $ rows)
 where
   interpretInner (label, term) = do
     control <.= term
     v <- interpret
     return (label, v)
   interpretWithValue = \case
     (Closure v b e) -> do
       curr <- get
       control <.= Lam v b
       let ctx = (curr ^. environment) & set bindings e
       environment <.= ctx
       interpret
     (VInt v) -> do
       control <.= I v
       interpret
     (VText v) -> do
       control <.= S v
       interpret
     (VBool b) -> do
       control <.= B b
       interpret
     r@(VRow values) -> do
       forM_ values $ \(_, v) -> interpretWithValue v
       interpret

run :: MInterpret a -> InterpreterState -> Either T.Text (a, InterpreterState)
run s = (runExcept . runStateT s)
