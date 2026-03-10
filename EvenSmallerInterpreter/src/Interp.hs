module Interp where

import qualified Data.Map.Strict as Map

type Symbol = String

data Value
    = NumV Float
    | BoolV Bool
    deriving (Show, Eq)

type Environment = Map.Map Symbol Value

emptyEnv :: Environment
emptyEnv = Map.empty

data Expr
    = NumE Float
    | BoolE Bool
    | PlusE Expr Expr
    | VarE Symbol
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
        _ -> error "Not adding numbers"
