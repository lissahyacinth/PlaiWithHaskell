module Interp where

import Data.Map (Map)
import qualified Data.Map as Map

emptyEnv :: Environment
emptyEnv = Map.empty

type Symbol = String

data Value
    = NumV Float
    | BoolV Bool
    | FunctionV Symbol Expr Environment
    deriving (Show, Eq)

type Environment = Map Symbol Value

data Expr
    = NumE Float
    | BoolE Bool
    | CondE Expr Expr Expr
    | VarE Symbol
    | PlusE Expr Expr
    | LamE Symbol Expr
    | AppE Expr Expr
    | Let1E Symbol Expr Expr
--    |Let1 Expr
    deriving (Show, Eq)

interp :: Expr -> Environment -> Value
interp (NumE n) _env = NumV n
interp (BoolE n) _env = BoolV n
interp (VarE sym) env =
    case Map.lookup sym env of
        Just v -> v
        Nothing -> error ("Unbound identifier: " ++ sym)

interp (PlusE lhs rhs) env =
    case (interp lhs env, interp rhs env) of
        (NumV a, NumV b) -> NumV (a + b)
        _ -> error "Not adding numbers" -- Painful, no type checker :(

interp (LamE sym expr) env = FunctionV sym expr env

interp (AppE fun arg) env =
    case interp fun env of
        FunctionV sym fn_body fnEnv -> interp fn_body (Map.insert sym (interp arg env) fnEnv)
        _ -> error "Type error, expected FunctionV"

interp (CondE testE thenE elseE) env =
    case interp testE env of
        BoolV True -> interp thenE env
        BoolV False -> interp elseE env
        _ -> error "Type Error, expected BoolV"

interp (Let1E variable value expr) env =
    interp expr (Map.insert variable (interp value env) env)
