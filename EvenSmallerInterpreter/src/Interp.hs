module Interp where

data BinOp
    = Plus
    | Concat
    | Division
    deriving (Show, Eq)

data Type
    = NumT
    | StrT
    deriving (Show, Eq)

data Value
    = NumV Float
    | StrV String
    deriving (Show, Eq)

data Expr
    = BinE BinOp Expr Expr
    | NumE Float
    | StrE String
    deriving (Show, Eq)

calc :: Expr -> Value
calc (NumE n) = NumV n
calc (StrE s) = StrV s
calc (BinE Division (NumE n) (NumE m)) = NumV(n / m)
calc (BinE Division _ _ ) = error "Type Error, Division requires two numbers"
calc (BinE Plus (NumE n) (NumE m)) = NumV(n + m)
calc (BinE Plus _ _) = error "Type Error, Plus requires two numbers"
calc (BinE Concat (StrE n) (StrE m)) = StrV(n ++ m)
calc (BinE Concat _ _) = error "Type Error, Concat requires two strings"


tc :: Expr -> Type
tc (BinE Plus lhs rhs) =
    case (tc lhs, tc rhs) of
        (NumT, NumT) -> NumT
        _ -> error "Type Error - Plus Requires Two Numbers"

tc (BinE Division lhs rhs) =
    case (tc lhs, tc rhs) of
        (NumT, NumT) -> NumT
        _ -> error "Type Error - Division Requires Two Numbers"


tc (BinE Concat lhs rhs) =
    case (tc lhs, tc rhs) of
        (StrT, StrT) -> StrT
        _ -> error "Type Error - Concat Requires Two Strings"

tc (NumE _) = NumT
tc (StrE _) = StrT
