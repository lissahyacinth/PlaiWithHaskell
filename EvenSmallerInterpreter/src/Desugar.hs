module Desugar (desugar, methodNameToMethodFn) where

import Types

methodNameToMethodFn :: String -> Expr
methodNameToMethodFn methodName = LamE "method" StrT (BinE Eq (StrE methodName) (VarE "method"))

-- Desugaring/MacrosmethodName
desugar :: Expr -> Expr
desugar Unreachable = Unreachable
-- e.g let sym: t = e1 in e2
desugar (Let1E sym t e1 e2) = AppE (desugar (LamE sym t e2)) (desugar e1)
desugar (BinE op l r) = BinE op (desugar l) (desugar r)
desugar (AppE f a) = AppE (desugar f) (desugar a)
desugar (LamE s t e) = LamE s t (desugar e)
desugar (CondE cond pos neg) = CondE (desugar cond) (desugar pos) (desugar neg)
desugar (NumE f) = NumE f
desugar (StrE s) = StrE s
desugar (VarE v) = VarE v
desugar (BoolE v) = BoolE v
desugar (DefineE className functionList env) =
  desugar
    ( Let1E
        className
        StrT
        ( LamE
            "methodName"
            StrT
            (SwitchE methodList (VarE "methodName"))
        )
        env
    )
    where methodList = zip (map (methodNameToMethodFn . fst) functionList) (map snd functionList)
-- Peel off the top item, add it to conditional with the next part as the remaining list
desugar (SwitchE conditionList conditionArg) =
  case conditionList of
    [(method, func)] ->
      desugar
        ( CondE
            (AppE method conditionArg)
            func
            Unreachable
        )
    ((method, func) : remainingFn) ->
      desugar
        ( CondE
            (AppE method conditionArg)
            func
            (SwitchE remainingFn conditionArg)
        )
    [] -> error "Unknown case"
