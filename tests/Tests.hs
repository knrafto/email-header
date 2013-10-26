module Main
    ( main
    ) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "tests" []

main :: IO ()
main = defaultMain tests
