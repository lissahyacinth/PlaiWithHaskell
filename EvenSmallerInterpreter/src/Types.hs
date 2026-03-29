module Types where

import Data.Map (Map)
import qualified Data.Map as Map

type Symbol = String

type Environment = Map Symbol StoredValue

type TypeEnvironment = Map Symbol Type

emptyEnv :: Environment
emptyEnv = Map.empty

emptyTEnv :: TypeEnvironment
emptyTEnv = Map.empty

data BinOp
  = Plus
  | Concat
  | Division
  | Eq
  deriving (Show, Eq, Enum)

data Type
  = NumT
  | StrT
  | VarT
  | BoolT
  | FunctionT Type Type -- Input Type -> Output Type
  deriving (Show, Eq)

data DataTag
  = TagNum
  | TagStr
  | TagBool
  | TagClosure
  | TagBinE
  | TagNumE
  | TagStrE
  | TagVarE
  | TagLamE
  | TagCondE
  | TagAppE
  | TagLet1E
  | TagDefineE
  | TagSwitchE
  | TagUnreachable
  | TagEnvE
  | TagNumT
  | TagStrT
  | TagVarT
  | TagBoolT
  | TagFunctionT
  deriving (Show, Eq, Enum)

data Value
  = NumV Float
  | StrV String
  | BoolV Bool
  | FunctionV Symbol Expr Environment -- Symbol, Body, Environment
  deriving (Show, Eq)

type MemoryAddress = Int

data StoredValue
  = NumSV MemoryAddress
  | StrSV MemoryAddress
  | BoolSV MemoryAddress
  | ClosureSV MemoryAddress
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
