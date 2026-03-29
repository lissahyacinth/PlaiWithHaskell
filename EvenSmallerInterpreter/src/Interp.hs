module Interp (calc) where

import qualified Data.Map as Map
import Store (Store, readStoredValue, storeValue, writeStoredValue)
import Types

calc :: Expr -> Store -> Environment -> IO StoredValue
calc (SwitchE _ _) _ _ = error "Should be desugared out"
calc (DefineE {}) _ _ = error "Should be desugared out"
calc Unreachable _ _ = error "Should be unreachable"
calc (NumE n) s _ = do
  (_, addr) <- storeValue s (NumV n)
  return (NumSV addr)
calc (StrE s) st _ = do
  (_, addr) <- storeValue st (StrV s)
  return (StrSV addr)
calc (BoolE e) st _ = do
  (_, addr) <- storeValue st (BoolV e)
  return (BoolSV addr)
calc (CondE cond pos neg) sto env = do
  val <- calc cond sto env >>= readStoredValue sto 
  case val of
    BoolV True -> calc pos sto env
    BoolV False -> calc neg sto env
    other -> error $ "Expected BoolV, received " ++ show other
calc (VarE sym) _ env = do
  case Map.lookup sym env of
    Just v -> return v
    Nothing -> error ("Unbound identifier: " ++ sym)
calc (LamE sym _ expr) memStore env = do
  (_, memAddr) <- storeValue memStore (FunctionV sym expr env)
  return (ClosureSV memAddr)
calc (AppE fun arg) sto env = do
  res <- calc fun sto env >>= readStoredValue sto
  argValue <- calc arg sto env
  case res of
    FunctionV sym fn_body fnEnv -> calc fn_body sto (Map.insert sym argValue fnEnv)
    _ -> error "Type error, expected FunctionV"
-- Calculation/Evaluation
calc (BinE Eq lhs rhs) sto env = do
  lhsEval <- calc lhs sto env >>= readStoredValue sto
  rhsEval <- calc rhs sto env >>= readStoredValue sto
  case (lhsEval, rhsEval) of
    (BoolV a, BoolV b) -> writeStoredValue sto (BoolV (a == b))
    (StrV a, StrV b) -> writeStoredValue sto (BoolV (a == b))
    (NumV a, NumV b) -> writeStoredValue sto (BoolV (a == b))
    _ -> error "Type Error"
calc (BinE Division (NumE n) (NumE m)) sto _ = do
  writeStoredValue sto (NumV (n / m))
calc (BinE Division lhs rhs) sto env = do
  lhsEval <- calc lhs sto env >>= readStoredValue sto
  rhsEval <- calc rhs sto env >>= readStoredValue sto
  case (lhsEval, rhsEval) of
    (NumV a, NumV b) -> writeStoredValue sto (NumV (a / b))
    other -> error $ "Expected NumV, found " ++ show other
calc (BinE Plus (NumE lhs) (NumE rhs)) sto _ = do
  writeStoredValue sto (NumV (lhs + rhs))
calc (BinE Plus lhs rhs) sto env = do
  lhsEval <- calc lhs sto env >>= readStoredValue sto
  rhsEval <- calc rhs sto env >>= readStoredValue sto
  case (lhsEval, rhsEval) of
    (NumV a, NumV b) -> writeStoredValue sto (NumV (a + b))
    other -> error $ "Expected NumV, found " ++ show other
calc (BinE Concat (StrE n) (StrE m)) sto _ = do
  writeStoredValue sto (StrV (n ++ m))
calc (BinE Concat lhs rhs) sto env = do
  lhsEval <- calc lhs sto env >>= readStoredValue sto
  rhsEval <- calc rhs sto env >>= readStoredValue sto
  case (lhsEval, rhsEval) of
    (StrV a, StrV b) -> writeStoredValue sto (StrV (a ++ b))
    _ -> error "Type error"
calc (Let1E {}) _ _ = error "Should be desugared out"
