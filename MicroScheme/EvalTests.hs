module MicroScheme.EvalTests (evalTests) where

import Test.HUnit

import MicroScheme.Eval
import MicroScheme.Parser
import MicroScheme.Value

assertEval expected input = do
  ast <- failOnParseError (parseSexp "<test case>" input)
  assertEqual ("evaluating " ++ show input) expected (eval ast)

evalIntTest = assertEval (IntValue 10) "10"
evalFloatTest = assertEval (FloatValue 2.0) "2.0"
evalBoolTest = assertEval (BoolValue True) "#t"

evalPlusIntIntTest = assertEval (IntValue 3) "(+ 1 2)"

evalTests =
    test [ "evalIntTest" ~: evalIntTest
         , "evalFloatTest" ~: evalFloatTest
         , "evalBoolTest" ~: evalBoolTest
         , "evalPlusIntIntTest" ~: evalPlusIntIntTest
         ]

