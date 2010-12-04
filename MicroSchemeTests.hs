module Main where

import Test.HUnit

import MicroScheme.AstTests
import MicroScheme.EvalTests
import MicroScheme.JitTests
import MicroScheme.ParserTests

tests = test [parserTests, astTests, evalTests, jitTests]
main = runTestTT tests
