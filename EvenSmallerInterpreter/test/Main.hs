import Test.Hspec
import Control.Exception (evaluate)
import Interp (Expr(..), Value(..), BinOp(..), Type(..), calc, tc, desugar, emptyEnv, emptyTEnv)

main :: IO ()
main = hspec $ do
    describe "addition" $ do
        it "adds two numbers" $
            calc (BinE Plus (NumE 1) (NumE 2)) emptyEnv `shouldBe` NumV 3
        it "concats two strings" $
            calc (BinE Concat (StrE "1") (StrE "2")) emptyEnv `shouldBe` StrV "12"
    describe "division" $ do
        it "divide two numbers" $
            calc (BinE Division (NumE 1) (NumE 2)) emptyEnv `shouldBe` NumV 0.5

    describe "lambdas and application" $ do
        it "identity function returns its argument" $
            -- (\x : Num -> x) 5
            calc (AppE (LamE "x" NumT (VarE "x")) (NumE 5)) emptyEnv `shouldBe` NumV 5
        it "constant function ignores second argument" $
            -- (\x : Num -> (\y : Num -> x)) 7 99
            calc (AppE (AppE (LamE "x" NumT (LamE "y" NumT (VarE "x"))) (NumE 7)) (NumE 99)) emptyEnv `shouldBe` NumV 7
        it "lambda with plus in body" $
            -- (\x : Num -> x + 1) 4
            calc (AppE (LamE "x" NumT (BinE Plus (VarE "x") (NumE 1))) (NumE 4)) emptyEnv `shouldBe` NumV 5
        it "nested application with plus" $
            -- (\f : Fn -> f 3) (\x : Num -> x + 10)
            calc (AppE (LamE "f" (FunctionT NumT NumT) (AppE (VarE "f") (NumE 3))) (LamE "x" NumT (BinE Plus (VarE "x") (NumE 10)))) emptyEnv `shouldBe` NumV 13
        it "applying non-function throws" $
            evaluate (calc (AppE (NumE 1) (NumE 2)) emptyEnv) `shouldThrow` anyException
        it "unbound variable throws" $
            evaluate (calc (VarE "x") emptyEnv) `shouldThrow` anyException
        it "lexical scoping - closure captures definition environment" $
            -- let x = 1 in (let f = (\y -> x + y) in (let x = 99 in f 0))
            -- should return 1, not 99
            let expr = AppE (LamE "x" NumT
                        (AppE (LamE "f" (FunctionT NumT NumT)
                            (AppE (LamE "x" NumT
                                (AppE (VarE "f") (NumE 0)))
                            (NumE 99)))
                        (LamE "y" NumT (BinE Plus (VarE "x") (VarE "y")))))
                      (NumE 1)
            in calc expr emptyEnv `shouldBe` NumV 1

    describe "desugaring Let1E" $ do
        it "let x : Num = 5 in x" $
            calc (desugar (Let1E "x" NumT (NumE 5) (VarE "x"))) emptyEnv `shouldBe` NumV 5
        it "let x : Num = 2 in x + 3" $
            calc (desugar (Let1E "x" NumT (NumE 2) (BinE Plus (VarE "x") (NumE 3)))) emptyEnv `shouldBe` NumV 5
        it "nested let - let x : Num = 1 in let y : Num = 2 in x + y" $
            calc (desugar (Let1E "x" NumT (NumE 1) (desugar (Let1E "y" NumT (NumE 2) (BinE Plus (VarE "x") (VarE "y")))))) emptyEnv `shouldBe` NumV 3
        it "let shadows outer binding" $
            let expr = desugar (Let1E "x" NumT (NumE 10)
                        (desugar (Let1E "x" NumT (NumE 20) (VarE "x"))))
            in calc expr emptyEnv `shouldBe` NumV 20

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
