module Main where

import Test.Hspec
import Interp (Expr(..),Value(..),interp, emptyEnv)

main :: IO ()
main = hspec $ do
    describe "addition" $ do
        it "adds two numbers" $
            interp (PlusE (NumE 1) (NumE 2)) emptyEnv `shouldBe` NumV 3

        it "adds nested expressions" $
            interp (PlusE (NumE 1) (PlusE (NumE 2) (NumE 3))) emptyEnv `shouldBe` NumV 6

    describe "assignment" $ do
        it "assigns and adds" $
            interp (Let1E "A" (NumE 2) (PlusE (NumE 1) (VarE "A"))) emptyEnv `shouldBe` NumV 3

    describe "conditionals" $ do
        it "tests positive path" $
            interp (CondE (BoolE True) (NumE 1) (NumE 2)) emptyEnv `shouldBe` NumV 1

        it "tests negative path" $
            interp (CondE (BoolE False) (NumE 1) (NumE 2)) emptyEnv `shouldBe` NumV 2

    describe "closures" $ do
        it "bindings" $
        -- x = 1
        -- f = lambda y: x
        -- x = 2
        -- f(10) -> Is x 2 or 1?
            interp (
                Let1E "'x" (NumE 1)
                    (Let1E "'f" (LamE "'y" (VarE "'x"))
                        (Let1E "'x" (NumE 2)
                            (AppE (VarE "'f") (NumE 10))
                        )
                    )
            ) emptyEnv `shouldBe` NumV 1

        it "closure_outside_definition_scope" $
            interp
                (AppE
                    (Let1E "'x" (NumE 3)
                        (LamE "'y" (PlusE (VarE "'x") (VarE "'y"))))
                    (NumE 4)
                )
                emptyEnv `shouldBe` NumV 7
