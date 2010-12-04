module Main where

import Test.HUnit

import MicroScheme.AstTests
import MicroScheme.EvalTests
import MicroScheme.ParserTests

tests = test [parserTests, astTests, evalTests]
main = runTestTT tests
