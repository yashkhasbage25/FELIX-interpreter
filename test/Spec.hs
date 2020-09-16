module Main where

import Test.HUnit
import TestEvaluate

main :: IO Counts

main = do
    runTestTT testsOpEval
