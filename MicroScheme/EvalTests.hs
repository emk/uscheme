module MicroScheme.EvalTests (evalTests) where

import Test.HUnit

import MicroScheme.Eval
import MicroScheme.Parser
import MicroScheme.Value

assertEval expected input = do
  ast <- failOnParseError (parseSexp "<test case>" input)
  actual <- eval ast
  assertEqual ("evaluating " ++ show input) expected actual

evalIntTest = assertEval (IntValue 10) "10"
evalFloatTest = assertEval (FloatValue 2.0) "2.0"
evalBoolTest = assertEval (BoolValue True) "#t"

evalPlusIntIntTest = assertEval (IntValue 3) "(+ 1 2)"
evalPlusIntFloatTest = assertEval (FloatValue 3.0) "(+ 1 2.0)"
evalPlusFloatIntTest = assertEval (FloatValue 3.0) "(+ 1.0 2)"
evalPlusFloatFloatTest = assertEval (FloatValue 3.0) "(+ 1.0 2.0)"

evalLetTest = assertEval (IntValue 2) "(let [[x 1]] (+ x x))"
evalLetOrderTest = assertEval (IntValue 2) "(let [[x 1] [x 2]] x)"
evalNestedLetTest =
    assertEval (IntValue 1) "(let [[x 1] [y 2]] (let [[x 3] [y x]] y))"

evalTests =
    test [ "evalIntTest" ~: evalIntTest
         , "evalFloatTest" ~: evalFloatTest
         , "evalBoolTest" ~: evalBoolTest
         , "evalPlusIntIntTest" ~: evalPlusIntIntTest
         , "evalPlusIntFloatTest" ~: evalPlusIntFloatTest
         , "evalPlusFloatIntTest" ~: evalPlusFloatIntTest
         , "evalPlusFloatFloatTest" ~: evalPlusFloatFloatTest
         , "evalLetTest" ~: evalLetTest
         , "evalLetOrderTest" ~: evalLetOrderTest
         , "evalNestedLetTest" ~: evalNestedLetTest
         ]

