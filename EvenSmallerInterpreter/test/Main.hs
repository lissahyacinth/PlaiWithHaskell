module Main where

import Test.Hspec
import Interp (Expr(..), Value(..), interp, emptyEnv)

main :: IO ()
main = hspec $ do
    describe "addition" $ do
        it "adds two numbers" $
            interp (PlusE (NumE 1) (NumE 2)) emptyEnv `shouldBe` NumV 3
