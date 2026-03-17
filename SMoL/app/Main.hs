module Main where

import Interp

main :: IO ()
main = do
    print $ calc (PlusE (NumE 1) (NumE 2))
    print $ calc (CatE (StrE "hello ") (StrE "world"))
