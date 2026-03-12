import Test.Hspec
import Control.Exception (evaluate)
import Interp (Expr(..), Value(..), BinOp(..), Type(..), calc, tc)

main :: IO ()
main = hspec $ do
    describe "addition" $ do
        it "adds two numbers" $
            calc (BinE Plus (NumE 1) (NumE 2)) `shouldBe` NumV 3
        it "concats two strings" $
            calc (BinE Concat (StrE "1") (StrE "2")) `shouldBe` StrV "12"
    describe "division" $ do
        it "divide two numbers" $
            calc (BinE Division (NumE 1) (NumE 2)) `shouldBe` NumV 0.5

    describe "type checking" $ do
        it "addition" $
            tc (BinE Plus (NumE 1) (NumE 2)) `shouldBe` NumT
        it "addition negative" $
            evaluate (tc (BinE Plus (StrE "1") (NumE 2))) `shouldThrow` anyException
        it "division" $
            tc (BinE Division (NumE 1) (NumE 2)) `shouldBe` NumT
        it "division negative" $
            evaluate (tc (BinE Division (StrE "1") (NumE 2))) `shouldThrow` anyException
        it "concats" $
            tc (BinE Concat (StrE "1") (StrE "2")) `shouldBe` StrT
        it "concats negative" $
            evaluate (tc (BinE Concat (NumE 1) (StrE "2"))) `shouldThrow` anyException
