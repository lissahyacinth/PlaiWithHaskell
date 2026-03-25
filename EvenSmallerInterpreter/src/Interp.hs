module Interp where

import Data.Map (Map)
import qualified Data.Map as Map

emptyEnv :: Environment
emptyEnv = Map.empty

emptyTEnv :: TypeEnvironment
emptyTEnv = Map.empty

type Symbol = String

type Environment = Map Symbol Value

type TypeEnvironment = Map Symbol Type

data BinOp
  = Plus
  | Concat
  | Division
  | Eq
  deriving (Show, Eq)

data Type
  = NumT
  | StrT
  | VarT
  | BoolT
  | FunctionT Type Type -- Input Type -> Output Type
  deriving (Show, Eq)

data Value
  = NumV Float
  | StrV String
  | BoolV Bool
  | FunctionV Symbol Expr Environment -- Symbol, Body, Environment
  deriving (Show, Eq)

data Expr
  = BinE BinOp Expr Expr
  | BoolE Bool
  | NumE Float
  | StrE String
  | VarE Symbol
  | LamE Symbol Type Expr
  | CondE Expr Expr Expr -- Conditional -> Bool IfTrue IfFalse
  | AppE Expr Expr -- Function, Argument (Calculate Function with Argument in Context)
  | Let1E Symbol Type Expr Expr -- Desugared out
  -- Class Name is constructed of Functions
  | DefineE Symbol [(String, Expr)] Expr -- Define X as a List of Expressions
  | SwitchE [(String, Expr)] Expr
  | Unreachable
  deriving (Show, Eq)

-- Desugaring/MacrosmethodName
desugar :: Expr -> Expr
desugar Unreachable = Unreachable
-- e.g let sym: t = e1 in e2
desugar (Let1E sym t e1 e2) = AppE (LamE sym t e2) e1
desugar (BinE op l r) = BinE op (desugar l) (desugar r)
desugar (AppE f a) = AppE (desugar f) (desugar a)
desugar (LamE s t e) = LamE s t (desugar e)
desugar (CondE cond pos neg) = CondE (desugar cond) (desugar pos) (desugar neg)
desugar (NumE f) = NumE f
desugar (StrE s) = StrE s
desugar (VarE v) = VarE v
desugar (BoolE v) = BoolE v
desugar (DefineE className functionList env) =
  -- Need an AppE to get something into the environment.
  -- But AppE only takes  (LamE -> FunctionV)
  -- Let ClassName = DispatchFunction in evaluation
  desugar
    ( Let1E
        className
        StrT
        ( desugar
            ( LamE
                "methodName"
                StrT
                (desugar (SwitchE functionList (VarE "methodName")))
            )
        )
        env
    )
-- Peel off the top item, add it to conditional with the next part as the remaining list
desugar (SwitchE conditionList conditionArg) =
  case conditionList of
    [(methodName, func)] ->
      desugar
        ( CondE
            (AppE (LamE "method" StrT (BinE Eq (StrE methodName) (VarE "method"))) conditionArg)
            func
            Unreachable
        )
    ((methodName, func) : remainingFn) ->
      desugar
        ( CondE
            (AppE (LamE "method" StrT (BinE Eq (StrE methodName) (VarE "method"))) conditionArg)
            func
            (SwitchE remainingFn conditionArg)
        )
    [] -> error "Unknown case"

calc :: Expr -> Environment -> Value
calc (SwitchE _ _) _ = error "Should be desugared out"
calc (DefineE {}) _ = error "Should be desugared out"
calc Unreachable _ = error "Should be unreachable"
calc (NumE n) _ = NumV n
calc (StrE s) _ = StrV s
calc (BoolE e) _ = BoolV e
calc (CondE cond pos neg) env =
  case calc cond env of
    BoolV True -> calc pos env
    BoolV False -> calc neg env
    FunctionV {} -> error "Received FunctionV, expected BoolV"
    _ -> error "Type Error, expected BoolV"
calc (VarE sym) env =
  case Map.lookup sym env of
    Just v -> v
    Nothing -> error ("Unbound identifier: " ++ sym)
calc (LamE sym _ expr) env = FunctionV sym expr env
calc (AppE fun arg) env =
  case calc fun env of
    FunctionV sym fn_body fnEnv -> calc fn_body (Map.insert sym (calc arg env) fnEnv)
    _ -> error "Type error, expected FunctionV"
-- Calculation/Evaluation
calc (BinE Eq lhs rhs) env =
  case (calc lhs env, calc rhs env) of
    (BoolV a, BoolV b) -> BoolV (a == b)
    (StrV a, StrV b) -> BoolV (a == b)
    (NumV a, NumV b) -> BoolV (a == b)
    _ -> error "Type Error"
calc (BinE Division (NumE n) (NumE m)) _ = NumV (n / m)
calc (BinE Division lhs rhs) env =
  case (calc lhs env, calc rhs env) of
    (NumV a, NumV b) -> NumV (a / b)
    _ -> error "Type error"
calc (BinE Plus (NumE n) (NumE m)) _ = NumV (n + m)
calc (BinE Plus lhs rhs) env =
  case (calc lhs env, calc rhs env) of
    (NumV a, NumV b) -> NumV (a + b)
    _ -> error "Type error"
calc (BinE Concat (StrE n) (StrE m)) _ = StrV (n ++ m)
calc (BinE Concat lhs rhs) env =
  case (calc lhs env, calc rhs env) of
    (StrV a, StrV b) -> StrV (a ++ b)
    _ -> error "Type error"
calc (Let1E {}) _ = error "Should be desugared out"

-- Type Checking
tc :: Expr -> TypeEnvironment -> Type
tc (BinE Plus lhs rhs) env =
  case (tc lhs env, tc rhs env) of
    (NumT, NumT) -> NumT
    _ -> error "Type Error - Plus Requires Two Numbers"
tc (BinE Division lhs rhs) env =
  case (tc lhs env, tc rhs env) of
    (NumT, NumT) -> NumT
    _ -> error "Type Error - Division Requires Two Numbers"
tc (BinE Concat lhs rhs) env =
  case (tc lhs env, tc rhs env) of
    (StrT, StrT) -> StrT
    _ -> error "Type Error - Concat Requires Two Strings"
tc (NumE _) _ = NumT
tc (StrE _) _ = StrT
tc (VarE sym) env =
  case Map.lookup sym env of
    Just v -> v
    Nothing -> error ("Unbound identifier: " ++ sym)
tc (LamE sym t1 expr) env =
  FunctionT t1 (tc expr (Map.insert sym t1 env))
tc (AppE fun arg) env =
  -- \|- C: U, |- A: U, |- B: D
  -- -------------------------
  -- AppE (LamE A B) C : D
  case tc fun env of
    FunctionT inType outType ->
      if tc arg env == inType
        then outType
        else error "Argument didn't match Argument Type"
    _ -> error "Expect function type within lambda"
tc (CondE cond pos neg) env =
  case tc cond env of
    BoolT
      | posT == negT -> posT
      | otherwise -> error "Expected positive and negative branches to share type"
    _ -> error "Expect Bool from conditional"
  where
    posT = tc pos env
    negT = tc neg env
tc (Let1E {}) _ = error "Should be desugared out"
