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

jitIntTest = assertJit (IntValue 1) "1"

jitTests =
    test [ "jitIntTest" ~: jitIntTest
         ]