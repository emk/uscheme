module Main where

import Test.HUnit

import MicroScheme.EvalTests
import MicroScheme.ParserTests

tests = test [parserTests, evalTests]
main = runTestTT tests
