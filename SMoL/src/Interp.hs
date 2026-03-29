module Interp where

import Control.Monad (forM_)
import Data.Char (chr, ord)
import Data.IORef (IORef, modifyIORef, readIORef)
import qualified Data.Vector.Mutable as MV

type MemoryAddress = Int

type MemoryValue = Int

data DataTag = TagNum | TagStr
  deriving (Show, Eq, Enum)

data Store = Store
  { memory :: MV.IOVector MemoryValue,
    nextAddr :: IORef Int
  }

-- Write a value to next available address, returning the address written to.
writeAndBump :: Store -> MemoryValue -> IO (Store, MemoryAddress)
writeAndBump (Store mem storeAddr) value = do
  addr <- readIORef storeAddr
  _ <- MV.write mem addr value
  _ <- modifyIORef storeAddr (+ 1)
  return (Store mem storeAddr, addr)

-- Store a numeric at next available address
storeNum :: Store -> Value -> IO (Store, MemoryAddress)
storeNum store (NumV value) = do
  (_, addr) <- writeAndBump store (fromEnum TagNum)
  _ <- writeAndBump store (truncate value) -- Bad, but do you want to do IEEE 754 here?
  return (store, addr)
storeNum _ _ = error "StoreNum only handles numbers"

-- Store a string at next available address
storeString :: Store -> Value -> IO (Store, MemoryAddress)
storeString store (StrV value) = do
  (_, addr) <- writeAndBump store (fromEnum TagStr)
  _ <- writeAndBump store strLen
  forM_ strOrd $ \c ->
    writeAndBump store c
  return (store, addr)
  where
    strLen = length value
    strOrd = map ord value
storeString _ _ = error "storeString only handles strings"

eval :: Store -> Expr -> IO Value
eval store expr = calc store expr >>= readStoredValue store

-- Get MemoryValue at given address
readMem :: Store -> MemoryAddress -> IO MemoryValue
readMem (Store mem _) = MV.read mem

readStoredValue :: Store -> StoredValue -> IO Value
readStoredValue store (NumSV s) = readValue store s
readStoredValue store (StrSV s) = readValue store s

-- Get Value at given address
readValue :: Store -> MemoryAddress -> IO Value
readValue store addr = do
  tagValue <- readMem store addr
  case toEnum tagValue of
    TagNum -> do
      val <- readMem store (addr + 1)
      return (NumV (fromIntegral val))
    TagStr -> do
      strLen <- readMem store (addr + 1)
      chars <- mapM (readMem store) [(addr + 2) .. (addr + 1 + strLen)]
      return (StrV (map chr chars))

data Expr
  = NumE Float
  | StrE String
  | PlusE Expr Expr
  | CatE Expr Expr
  deriving (Show, Eq)

data Value
  = NumV Float
  | StrV String
  deriving (Show, Eq)

data StoredValue
  = NumSV MemoryAddress
  | StrSV MemoryAddress
  deriving (Show, Eq)

numPlus :: Store -> StoredValue -> StoredValue -> IO StoredValue
numPlus store (NumSV l) (NumSV r) = do
  lhsSValue <- readValue store l
  rhsSValue <- readValue store r
  case (lhsSValue, rhsSValue) of
    (NumV lhsValue, NumV rhsValue) -> do
      (_, addr) <- storeNum store (NumV (lhsValue + rhsValue))
      return (NumSV addr)
    _ -> error "Did not find Numerics at StoredValue - this is basically a segfault"
numPlus _ _ _ = error "Expects two numbers"

strCat :: Store -> StoredValue -> StoredValue -> IO StoredValue
strCat store (StrSV l) (StrSV r) = do
  lhsSValue <- readValue store l
  rhsSValue <- readValue store r
  case (lhsSValue, rhsSValue) of
    (StrV lhsValue, StrV rhsValue) -> do
      (_, addr) <- storeString store (StrV (lhsValue ++ rhsValue))
      return (StrSV addr)
    _ -> error "Did not find Strings at StoredValue - this is basically a segfault"
strCat _ _ _ = error "Expects two strings"

calc :: Store -> Expr -> IO StoredValue
calc store (NumE n) = do
  (_, addr) <- storeNum store (NumV n)
  return (NumSV addr)
calc store (StrE s) = do
  (_, addr) <- storeString store (StrV s)
  return (StrSV addr)
calc store (PlusE l r) = do
  lhs <- calc store l
  rhs <- calc store r
  numPlus store lhs rhs
calc store (CatE l r) = do
  lhs <- calc store l
  rhs <- calc store r
  strCat store lhs rhs
