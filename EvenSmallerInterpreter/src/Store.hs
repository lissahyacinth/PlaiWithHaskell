module Store where

import Control.Monad (forM, forM_, void)
import Data.Char (chr, ord)
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Map (size)
import qualified Data.Map as Map
import qualified Data.Vector.Mutable as MV
import Types

type MemoryValue = Int

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

storeValue :: Store -> Value -> IO (Store, MemoryAddress)
-- Store a numeric at next available address
storeValue memStore (NumV value) = do
  let (sig, ex) = decodeFloat value 
  (_, addr) <- writeAndBump memStore (fromEnum TagNum)
  void $ writeAndBump memStore (fromInteger sig)
  void $ writeAndBump memStore ex
  return (memStore, addr)

-- Store a string at next available address
storeValue memStore (StrV value) = do
  (_, addr) <- writeAndBump memStore (fromEnum TagStr)
  _ <- writeAndBump memStore strLen
  forM_ strOrd $ \c ->
    writeAndBump memStore c
  return (memStore, addr)
  where
    strLen = length value
    strOrd = map ord value

-- Store a boolean at next available address
storeValue memStore (BoolV value) = do
  (_, addr) <- writeAndBump memStore (fromEnum TagBool)
  _ <- writeAndBump memStore (fromEnum value)
  return (memStore, addr)

-- Store a Closure at the next available address
storeValue memStore (FunctionV symbol expr env) = do
  (_, symbolAddr) <- storeValue memStore (StrV symbol)
  exprAddr <- storeExpr memStore expr
  envAddr <- storeEnv memStore env
  -- Write Tag after items to write in a single block
  tagAddr <- snd <$> writeAndBump memStore (fromEnum TagClosure)
  void $ writeAndBump memStore symbolAddr
  void $ writeAndBump memStore exprAddr
  void $ writeAndBump memStore envAddr
  return (memStore, tagAddr)

storeType :: Store -> Type -> IO MemoryAddress
storeType memStore StrT = do 
  snd <$> writeAndBump memStore (fromEnum TagStrT)
storeType memStore NumT = do 
  snd <$> writeAndBump memStore (fromEnum TagNumT)
storeType memStore VarT = do 
  snd <$> writeAndBump memStore (fromEnum TagVarT)
storeType memStore BoolT = do 
  snd <$> writeAndBump memStore (fromEnum TagBoolT)
storeType memStore (FunctionT a b) = do 
  typeAAddr <- storeType memStore a
  typeBAddr <- storeType memStore b
  tagAddr <- snd <$> writeAndBump memStore (fromEnum TagFunctionT)
  void $ writeAndBump memStore typeAAddr 
  void $ writeAndBump memStore typeBAddr 
  return tagAddr

readType :: Store -> MemoryAddress -> IO Type 
readType memStore addr = do 
  typeEnum <- readMemValue memStore addr
  case toEnum typeEnum of
    TagNumT -> return NumT
    TagVarT -> return VarT
    TagBoolT ->return BoolT
    TagStrT -> return StrT
    TagFunctionT -> do 
      typeA <- followPtr memStore (addr + 1) readType
      typeB <- followPtr memStore (addr + 2) readType
      return (FunctionT typeA typeB)
    other -> error $ "Expected a TagValue, found " ++ show other

storeExpr :: Store -> Expr -> IO MemoryAddress
storeExpr memStore (BinE op a b) = do
  exprAAddr <- storeExpr memStore a
  exprBAddr <- storeExpr memStore b
  (_, tagAddr) <- writeAndBump memStore (fromEnum TagBinE)
  _ <- writeAndBump memStore (fromEnum op)
  _ <- writeAndBump memStore exprAAddr
  _ <- writeAndBump memStore exprBAddr
  return tagAddr -- [0; TagAddr, 1; ->exprAddr, 2; ->exprAddr]
storeExpr memStore (NumE x) = do
  numAddr <- snd <$> storeValue memStore (NumV x)
  tagAddr <- snd <$> writeAndBump memStore (fromEnum TagNumE)
  void $ writeAndBump memStore numAddr
  return tagAddr -- [0; tagAddr, 1; ->numAddr]
storeExpr memStore (VarE sym) = do
  strAddr <- snd <$> storeValue memStore (StrV sym)
  tagAddr <- snd <$> writeAndBump memStore (fromEnum TagVarE)
  void $ writeAndBump memStore strAddr
  return tagAddr -- [0; tagAddr, 1; ->strAddr]
storeExpr memStore (StrE s) = do 
  (_, strAddr) <- storeValue memStore (StrV s)
  (_, tagAddr) <- writeAndBump memStore (fromEnum TagStrE)
  _ <- writeAndBump memStore strAddr
  return tagAddr -- [0; TagAddr, 1; ->strAddr]
storeExpr memStore (AppE e1 e2) = do 
  exprAAddr <- storeExpr memStore e1
  exprBAddr <- storeExpr memStore e2
  tagAddr <- snd <$> writeAndBump memStore (fromEnum TagAppE)
  void $ writeAndBump memStore exprAAddr
  void $ writeAndBump memStore exprBAddr
  return tagAddr -- [0; TagAddr, 1; Pointer -> ExprA, 2; Pointer -> ExprB]
storeExpr memStore (CondE cond pos neg) = do 
  exprAAddr <- storeExpr memStore cond
  exprBAddr <- storeExpr memStore pos
  exprCAddr <- storeExpr memStore neg
  tagAddr <- snd <$> writeAndBump memStore (fromEnum TagCondE)
  void $ writeAndBump memStore exprAAddr
  void $ writeAndBump memStore exprBAddr
  void $ writeAndBump memStore exprCAddr
  return tagAddr -- [0; TagAddr, 1; Pointer -> ExprA, 2; Pointer -> ExprB, 3; Pointer -> ExprC]
storeExpr memStore (LamE sym t1 e1) = do 
  symAddr <- snd <$> storeValue memStore (StrV sym)
  typeAddr <- storeType memStore t1 
  exprAddr <- storeExpr memStore e1 
  tagAddr <- snd <$> writeAndBump memStore (fromEnum TagLamE)
  void $ writeAndBump memStore symAddr
  void $ writeAndBump memStore typeAddr
  void $ writeAndBump memStore exprAddr
  return tagAddr -- [0; TagAddr, 1; ->Sym, 2; ->Type, 3; ->expr]
storeExpr memStore Unreachable = do 
  snd <$> writeAndBump memStore (fromEnum TagUnreachable) -- [0; TagAddr]

storeExpr _ other = do
  error $ "No impl for " ++ show other

readEnv :: Store -> MemoryAddress -> IO Environment
readEnv memStore addr = do
  envSize <- readMemValue memStore (addr + 1)
  pairs <- forM [0, 2 .. (envSize * 2 - 2)] $ \i ->
    do
      k <- followPtr memStore (addr + i + 2) readValue
      v <- followPtr memStore (addr + i + 3) addrToStoredValue
      case k of
        StrV s -> return (s, v)
        _ -> error "Expected Symbol"
  return (Map.fromList pairs)

storeEnv :: Store -> Environment -> IO MemoryAddress
storeEnv memStore env = do
  envAddrs <-
    mapM
      ( \(k, v) ->
          do
            kAddr <- snd <$> storeValue memStore (StrV k)
            return (kAddr, storedValueToAddr v)
      )
      (Map.toList env)
  -- Store Pointers in the allocated spaces for the environment
  memAddr <- snd <$> writeAndBump memStore (fromEnum TagEnvE)
  void $ writeAndBump memStore (size env) -- Key + Address
  forM_ envAddrs $ \(k, v) -> do
    void $ writeAndBump memStore k -- Pointer to K
    void $ writeAndBump memStore v -- Pointer to V
  return memAddr

-- Get MemoryValue at given address
readMemValue :: Store -> MemoryAddress -> IO MemoryValue
readMemValue (Store mem _) = MV.read mem

followPtr :: Store -> MemoryAddress -> (Store -> MemoryAddress -> IO a) -> IO a
followPtr memStore addr reader = do
  ptr <- readMemValue memStore addr
  reader memStore ptr

readStoredValue :: Store -> StoredValue -> IO Value
readStoredValue memStore (NumSV s) = readValue memStore s
readStoredValue memStore (StrSV s) = readValue memStore s
readStoredValue memStore (BoolSV s) = readValue memStore s
readStoredValue memStore (ClosureSV s) = readValue memStore s

storedValueToAddr :: StoredValue -> MemoryAddress
storedValueToAddr (NumSV addr) = addr 
storedValueToAddr (StrSV addr) = addr 
storedValueToAddr (BoolSV addr) = addr 
storedValueToAddr (ClosureSV addr) = addr 

addrToStoredValue :: Store -> MemoryAddress -> IO StoredValue
addrToStoredValue memStore addr = do 
  tagValue <- readMemValue memStore addr 
  return $ case toEnum tagValue of 
    TagNum -> NumSV addr 
    TagStr -> StrSV addr
    TagBool -> BoolSV addr 
    TagClosure -> ClosureSV addr
    other -> error $ "Expected an addr that resolves to a Stored Value, found " ++ show other

writeStoredValue :: Store -> Value -> IO StoredValue
writeStoredValue memStore value = do
  addr <- snd <$> storeValue memStore value
  return $ case value of
    NumV _ -> NumSV addr
    StrV _ -> StrSV addr
    BoolV _ -> BoolSV addr
    FunctionV {} -> ClosureSV addr

-- Get Value at given address
readValue :: Store -> MemoryAddress -> IO Value
readValue memStore addr = do
  tagValue <- readMemValue memStore addr
  case toEnum tagValue of
    TagNum -> do
      sig <- readMemValue memStore (addr + 1)
      expon <- readMemValue memStore (addr + 2)
      return (NumV (encodeFloat (toInteger sig) expon))
    TagStr -> do
      strLen <- readMemValue memStore (addr + 1)
      chars <- mapM (readMemValue memStore) [(addr + 2) .. (addr + 1 + strLen)]
      return (StrV (map chr chars))
    TagBool -> do
      val <- readMemValue memStore (addr + 1)
      return (BoolV (toEnum val))
    TagClosure -> do
      closureName <- followPtr memStore (addr + 1) readValue
      expr <- followPtr memStore (addr + 2) readExpr
      env <- followPtr memStore (addr + 3) readEnv
      case closureName of
        (StrV sym) -> return (FunctionV sym expr env)
        _ -> error "Expected symbol, found something else"
    _ -> error "This covers all values"

readExpr :: Store -> MemoryAddress -> IO Expr
readExpr memStore addr = do
  exprValue <- readMemValue memStore addr
  case toEnum exprValue of
    TagUnreachable -> return Unreachable
    TagAppE -> do 
      expr1 <- followPtr memStore (addr + 1) readExpr -- Stored as Expr
      expr2 <- followPtr memStore (addr + 2) readExpr -- Stored as Expr
      return (AppE expr1 expr2)
    TagLamE -> do 
      sym1 <- followPtr memStore (addr + 1) readValue -- Stored as StrV
      type1 <- followPtr memStore (addr + 2) readType -- Stored as Type
      expr1 <- followPtr memStore (addr + 3) readExpr -- Stored as Expr
      case sym1 of 
        (StrV sym) -> return (LamE sym type1 expr1)
        other -> error $ "Expected StrV within symbol memory address store, found " ++ show other
    TagBinE -> do
      opBin <- fmap toEnum (readMemValue memStore (addr + 1))
      expr1 <- followPtr memStore (addr + 2) readExpr 
      expr2 <- followPtr memStore (addr + 3) readExpr 
      return (BinE opBin expr1 expr2)
    TagCondE -> do 
      expr1 <- followPtr memStore (addr + 1) readExpr 
      expr2 <- followPtr memStore (addr + 2) readExpr 
      expr3 <- followPtr memStore (addr + 3) readExpr 
      return (CondE expr1 expr2 expr3)

    TagStrE -> do
      str <- followPtr memStore (addr + 1) readValue
      case str of
        StrV s -> return (StrE s)
        other -> error $ "Expected NumV, received " ++ show other
    TagNumE -> do
      num <- followPtr memStore (addr + 1) readValue
      case num of
        NumV n -> return (NumE n)
        other -> error $ "Expected NumV, received " ++ show other
    TagVarE -> do
      val <- followPtr memStore (addr + 1) readValue -- Stored as StrV
      case val of
        (StrV v) -> return (VarE v)
        other -> error $ "Expected String in Value, found " ++ show other
    other -> error $ "Can't handle " ++ show other
