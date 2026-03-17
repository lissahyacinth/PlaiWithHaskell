import Test.Hspec
import Control.Exception (evaluate)
import Data.IORef (newIORef)
import qualified Data.Vector.Mutable as MV
import Interp (Expr(..), Value(..), Store(..), calc, eval,
               storeNum, storeString, readValue, readMem, writeAndBump)

mkStore :: IO Store
mkStore = do
    mem <- MV.replicate 100 (-1)
    addr <- newIORef 0
    return (Store mem addr)

main :: IO ()
main = hspec $ do
    describe "calc" $ do
        it "evaluates a number" $ do
            store <- mkStore
            result <- eval store (NumE 42)
            result `shouldBe` NumV 42
        it "evaluates a string" $ do
            store <- mkStore
            result <- eval store (StrE "hi")
            result `shouldBe` StrV "hi"

    describe "addition (safety)" $ do
        it "adds two numbers" $ do
            store <- mkStore
            result <- eval store (PlusE (NumE 1) (NumE 2))
            result `shouldBe` NumV 3
        it "rejects string on left" $ do
            store <- mkStore
            calc store (PlusE (StrE "a") (NumE 1)) `shouldThrow` anyException
        it "rejects string on right" $ do
            store <- mkStore
            calc store (PlusE (NumE 1) (StrE "b")) `shouldThrow` anyException
        it "rejects two strings" $ do
            store <- mkStore
            calc store (PlusE (StrE "a") (StrE "b")) `shouldThrow` anyException

    describe "concatenation (safety)" $ do
        it "concatenates two strings" $ do
            store <- mkStore
            result <- eval store (CatE (StrE "hello ") (StrE "world"))
            result `shouldBe` StrV "hello world"
        it "rejects number on left" $ do
            store <- mkStore
            calc store (CatE (NumE 1) (StrE "b")) `shouldThrow` anyException
        it "rejects number on right" $ do
            store <- mkStore
            calc store (CatE (StrE "a") (NumE 1)) `shouldThrow` anyException
        it "rejects two numbers" $ do
            store <- mkStore
            calc store (CatE (NumE 1) (NumE 2)) `shouldThrow` anyException

    describe "nested expressions" $ do
        it "nested addition" $ do
            store <- mkStore
            result <- eval store (PlusE (PlusE (NumE 1) (NumE 2)) (NumE 3))
            result `shouldBe` NumV 6
        it "nested concatenation" $ do
            store <- mkStore
            result <- eval store (CatE (CatE (StrE "a") (StrE "b")) (StrE "c"))
            result `shouldBe` StrV "abc"
        it "safety in nested - plus inside cat" $ do
            store <- mkStore
            eval store (CatE (PlusE (NumE 1) (NumE 2)) (StrE "c")) `shouldThrow` anyException
        it "safety in nested - cat inside plus" $ do
            store <- mkStore
            eval store (PlusE (CatE (StrE "a") (StrE "b")) (NumE 1)) `shouldThrow` anyException

    describe "store - writeAndBump" $ do
        it "writes to address 0 first" $ do
            store <- mkStore
            (_, addr) <- writeAndBump store 42
            addr `shouldBe` 0
        it "bumps address after write" $ do
            store <- mkStore
            _ <- writeAndBump store 42
            (_, addr) <- writeAndBump store 99
            addr `shouldBe` 1
        it "value is readable at written address" $ do
            store <- mkStore
            (_, addr) <- writeAndBump store 42
            val <- readMem store addr
            val `shouldBe` 42

    describe "store - storeNum" $ do
        it "stores and reads back a number" $ do
            store <- mkStore
            (_, addr) <- storeNum store (NumV 7)
            val <- readValue store addr
            val `shouldBe` NumV 7
        it "stores tag 0 for numbers" $ do
            store <- mkStore
            (_, addr) <- storeNum store (NumV 5)
            tag <- readMem store addr
            tag `shouldBe` 0
        it "rejects non-number" $ do
            store <- mkStore
            (evaluate =<< storeNum store (StrV "hi")) `shouldThrow` anyException

    describe "store - storeString" $ do
        it "stores and reads back a string" $ do
            store <- mkStore
            (_, addr) <- storeString store (StrV "hi")
            val <- readValue store addr
            val `shouldBe` StrV "hi"
        it "stores tag 1 for strings" $ do
            store <- mkStore
            (_, addr) <- storeString store (StrV "hi")
            tag <- readMem store addr
            tag `shouldBe` 1
        it "stores correct length" $ do
            store <- mkStore
            (_, addr) <- storeString store (StrV "hello")
            len <- readMem store (addr + 1)
            len `shouldBe` 5
        it "stores empty string" $ do
            store <- mkStore
            (_, addr) <- storeString store (StrV "")
            val <- readValue store addr
            val `shouldBe` StrV ""
        it "rejects non-string" $ do
            store <- mkStore
            (evaluate =<< storeString store (NumV 5)) `shouldThrow` anyException

    describe "store - multiple values" $ do
        it "stores number then string, reads both back" $ do
            store <- mkStore
            (_, addr1) <- storeNum store (NumV 42)
            (_, addr2) <- storeString store (StrV "abc")
            v1 <- readValue store addr1
            v2 <- readValue store addr2
            v1 `shouldBe` NumV 42
            v2 `shouldBe` StrV "abc"
