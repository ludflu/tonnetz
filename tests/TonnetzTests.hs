module Main where

import Data.Foldable
import Data.List.Unique (sortUniq)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import NeoRiemann
import Progressions

import ProgressionsTests




allTests :: TestTree
allTests = testGroup "All Tests" [progTests]

main :: IO ()
main = defaultMain allTests
