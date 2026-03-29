module TypeCheck (tc) where

import qualified Data.Map as Map
import Types

-- Type Checking
tc :: Expr -> TypeEnvironment -> Type
tc (BinE Plus lhs rhs) env =
  case (tc lhs env, tc rhs env) of
    (NumT, NumT) -> NumT
    _ -> error "Type Error - Plus Requires Two Numbers"
tc (BinE Eq lhs rhs) env =
  case (tc lhs env, tc rhs env) of
    (BoolT, BoolT) -> BoolT
    (NumT, NumT) -> BoolT
    (StrT, StrT) -> BoolT
    _ -> error "Type Error - Eq Requires items that resolve to Bool"
tc (BinE Division lhs rhs) env =
  case (tc lhs env, tc rhs env) of
    (NumT, NumT) -> NumT
    _ -> error "Type Error - Division Requires Two Numbers"
tc (BinE Concat lhs rhs) env =
  case (tc lhs env, tc rhs env) of
    (StrT, StrT) -> StrT
    _ -> error "Type Error - Concat Requires Two Strings"
tc (SwitchE {}) _ = error "Should be desugared out"
tc (DefineE {}) _ = error "Should be desugared out"
tc Unreachable _ = error "Should be unreachable"
tc (BoolE _) _ = BoolT
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
