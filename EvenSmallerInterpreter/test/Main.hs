import Control.Exception (evaluate)
import Data.IORef (newIORef)
import qualified Data.Map as Map
import qualified Data.Vector.Mutable as MV
import Desugar (desugar)
import Interp (calc)
import Store
import Test.Hspec
import TypeCheck (tc)
import Types

mkStore :: IO Store
mkStore = do
  mem <- MV.new 1024
  addr <- newIORef 0
  return (Store mem addr)

-- Run calc and resolve the StoredValue back to a Value
run :: Expr -> Environment -> IO Value
run expr env = do
  s <- mkStore
  sv <- calc expr s env
  readStoredValue s sv

main :: IO ()
main = hspec $ do
  describe "addition" $ do
    it "adds two numbers" $
      run (BinE Plus (NumE 1) (NumE 2)) emptyEnv `shouldReturn` NumV 3
    it "concats two strings" $
      run (BinE Concat (StrE "1") (StrE "2")) emptyEnv `shouldReturn` StrV "12"
  describe "division" $ do
    it "divide two numbers" $
      run (BinE Division (NumE 1) (NumE 2)) emptyEnv `shouldReturn` NumV 0.5

  describe "lambdas and application" $ do
    it "identity function returns its argument" $
      -- (\x : Num -> x) 5
      run (AppE (LamE "x" NumT (VarE "x")) (NumE 5)) emptyEnv `shouldReturn` NumV 5
    it "constant function ignores second argument" $
      -- (\x : Num -> (\y : Num -> x)) 7 99
      run (AppE (AppE (LamE "x" NumT (LamE "y" NumT (VarE "x"))) (NumE 7)) (NumE 99)) emptyEnv `shouldReturn` NumV 7
    it "lambda with plus in body" $
      -- (\x : Num -> x + 1) 4
      run (AppE (LamE "x" NumT (BinE Plus (VarE "x") (NumE 1))) (NumE 4)) emptyEnv `shouldReturn` NumV 5
    it "nested application with plus" $
      -- (\f : Fn -> f 3) (\x : Num -> x + 10)
      run (AppE (LamE "f" (FunctionT NumT NumT) (AppE (VarE "f") (NumE 3))) (LamE "x" NumT (BinE Plus (VarE "x") (NumE 10)))) emptyEnv `shouldReturn` NumV 13
    it "applying non-function throws" $
      run (AppE (NumE 1) (NumE 2)) emptyEnv `shouldThrow` anyException
    it "unbound variable throws" $
      run (VarE "x") emptyEnv `shouldThrow` anyException
    it "lexical scoping - closure captures definition environment" $
      -- let x = 1 in (let f = (\y -> x + y) in (let x = 99 in f 0))
      -- should return 1, not 99
      let expr =
            AppE
              ( LamE
                  "x"
                  NumT
                  ( AppE
                      ( LamE
                          "f"
                          (FunctionT NumT NumT)
                          ( AppE
                              ( LamE
                                  "x"
                                  NumT
                                  (AppE (VarE "f") (NumE 0))
                              )
                              (NumE 99)
                          )
                      )
                      (LamE "y" NumT (BinE Plus (VarE "x") (VarE "y")))
                  )
              )
              (NumE 1)
       in run expr emptyEnv `shouldReturn` NumV 1

  describe "Calculating Switch" $ do
    it "desugars and calculates a conditionList" $
      run
        ( desugar
            ( SwitchE
                [ ("method1", Let1E "x" NumT (NumE 5) (VarE "x")),
                  ("method2", Let1E "x" NumT (NumE 6) (VarE "x"))
                ]
                (StrE "method1")
            )
        )
        emptyEnv
        `shouldReturn` NumV 5
    it "desugars a classE" $
      run
        ( desugar
            ( DefineE
                "myClass"
                [ ("method1", Let1E "x" NumT (NumE 5) (VarE "x")),
                  ("method2", Let1E "x" NumT (NumE 6) (VarE "x"))
                ]
                ( AppE
                    (VarE "myClass")
                    (StrE "method1")
                )
            )
        )
        emptyEnv
        `shouldReturn` NumV 5

  describe "desugaring Let1E" $ do
    it "let x : Num = 5 in x" $
      run (desugar (Let1E "x" NumT (NumE 5) (VarE "x"))) emptyEnv `shouldReturn` NumV 5
    it "let x : Num = 2 in x + 3" $
      run (desugar (Let1E "x" NumT (NumE 2) (BinE Plus (VarE "x") (NumE 3)))) emptyEnv `shouldReturn` NumV 5
    it "nested let - let x : Num = 1 in let y : Num = 2 in x + y" $
      run (desugar (Let1E "x" NumT (NumE 1) (desugar (Let1E "y" NumT (NumE 2) (BinE Plus (VarE "x") (VarE "y")))))) emptyEnv `shouldReturn` NumV 3
    it "let shadows outer binding" $
      let expr =
            desugar
              ( Let1E
                  "x"
                  NumT
                  (NumE 10)
                  (desugar (Let1E "x" NumT (NumE 20) (VarE "x")))
              )
       in run expr emptyEnv `shouldReturn` NumV 20

  describe "type checking" $ do
    it "addition" $
      tc (BinE Plus (NumE 1) (NumE 2)) emptyTEnv `shouldBe` NumT
    it "addition negative" $
      evaluate (tc (BinE Plus (StrE "1") (NumE 2)) emptyTEnv) `shouldThrow` anyException
    it "division" $
      tc (BinE Division (NumE 1) (NumE 2)) emptyTEnv `shouldBe` NumT
    it "division negative" $
      evaluate (tc (BinE Division (StrE "1") (NumE 2)) emptyTEnv) `shouldThrow` anyException
    it "concats" $
      tc (BinE Concat (StrE "1") (StrE "2")) emptyTEnv `shouldBe` StrT
    it "concats negative" $
      evaluate (tc (BinE Concat (NumE 1) (StrE "2")) emptyTEnv) `shouldThrow` anyException

  describe "type checking lambdas and application" $ do
    it "identity on number - (\\x : Num -> x) 5" $
      tc (AppE (LamE "x" NumT (VarE "x")) (NumE 5)) emptyTEnv `shouldBe` NumT
    it "identity on string - (\\x : Str -> x) \"hi\"" $
      tc (AppE (LamE "x" StrT (VarE "x")) (StrE "hi")) emptyTEnv `shouldBe` StrT
    it "lambda with plus - (\\x : Num -> x + 1) 4" $
      tc (AppE (LamE "x" NumT (BinE Plus (VarE "x") (NumE 1))) (NumE 4)) emptyTEnv `shouldBe` NumT
    it "argument type mismatch errors - (\\x : Num -> x + 1) \"hi\"" $
      evaluate (tc (AppE (LamE "x" NumT (BinE Plus (VarE "x") (NumE 1))) (StrE "hi")) emptyTEnv) `shouldThrow` anyException
    it "applying non-function errors" $
      evaluate (tc (AppE (NumE 1) (NumE 2)) emptyTEnv) `shouldThrow` anyException
    it "unbound variable errors" $
      evaluate (tc (VarE "x") emptyTEnv) `shouldThrow` anyException
    it "lambda type is FunctionT" $
      tc (LamE "x" NumT (BinE Plus (VarE "x") (NumE 1))) emptyTEnv `shouldBe` FunctionT NumT NumT
    it "nested application - (\\f : Num->Num -> f 3) (\\x : Num -> x + 10)" $
      tc (AppE (LamE "f" (FunctionT NumT NumT) (AppE (VarE "f") (NumE 3))) (LamE "x" NumT (BinE Plus (VarE "x") (NumE 10)))) emptyTEnv `shouldBe` NumT
    it "desugared let typechecks - let x : Num = 5 in x + 1" $
      tc (desugar (Let1E "x" NumT (NumE 5) (BinE Plus (VarE "x") (NumE 1)))) emptyTEnv `shouldBe` NumT
    it "desugared let catches type error - let x : Num = \"hi\" in x + 1" $
      evaluate (tc (desugar (Let1E "x" NumT (StrE "hi") (BinE Plus (VarE "x") (NumE 1)))) emptyTEnv) `shouldThrow` anyException

  describe "store round-trip" $ do
    it "stores and loads a NumV" $ do
      s <- mkStore
      (_, addr) <- storeValue s (NumV 42)
      val <- readValue s addr
      val `shouldBe` NumV 42

    it "stores and loads a StrV" $ do
      s <- mkStore
      (_, addr) <- storeValue s (StrV "hello")
      val <- readValue s addr
      val `shouldBe` StrV "hello"

    it "stores and loads a BoolV" $ do
      s <- mkStore
      (_, addr) <- storeValue s (BoolV True)
      val <- readValue s addr
      val `shouldBe` BoolV True

    it "stores and loads a FunctionV with empty env" $ do
      s <- mkStore
      let closure = FunctionV "x" (BinE Plus (VarE "x") (NumE 1)) Map.empty
      (_, addr) <- storeValue s closure
      val <- readValue s addr
      val `shouldBe` closure

    it "stores and loads a FunctionV with captured env" $ do
      s <- mkStore
      (_, yAddr) <- storeValue s (NumV 10)
      let env = Map.fromList [("y", NumSV yAddr)]
      let closure = FunctionV "x" (BinE Plus (VarE "x") (VarE "y")) env
      (_, addr) <- storeValue s closure
      val <- readValue s addr
      val `shouldBe` closure
