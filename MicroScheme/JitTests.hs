module MicroScheme.JitTests (jitTests) where

import Test.HUnit

import MicroScheme.Ast
import MicroScheme.Jit
import MicroScheme.Parser
import MicroScheme.Value

assertJit expected input = do
    sexp <- failOnParseError (parseSexp "<test case>" input)
    actual <- jit (buildAst sexp)
    assertEqual ("jitting " ++ show input) expected actual

jitInt1Test = assertJit (IntValue 1) "1"
jitInt2Test = assertJit (IntValue 2) "2"

jitTests =
    test [ "jitInt1Test" ~: jitInt1Test
         , "jitInt2Test" ~: jitInt2Test
         ]