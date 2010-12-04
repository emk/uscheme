module MicroScheme.AstTests (astTests) where

import Test.HUnit

import MicroScheme.Ast
import MicroScheme.Parser
import MicroScheme.Value

assertAst expected input = do
  sexp <- failOnParseError (parseSexp "<test case>" input)
  assertEqual ("building ast for " ++ show input) expected (buildAst sexp)

astLiteralTest = assertAst (Literal (IntValue 1)) "1"
astPrimitiveTest = assertAst (Primitive "+" [Literal (IntValue 1),
                                             Literal (IntValue 2)])
                             "(+ 1 2)"
astLetTest = assertAst (Let [("x", Literal (IntValue 1))]
                            (Body [Literal (IntValue 2),
                                   Var "x"]))
                       "(let [[x 1]] 2 x)"

astTests =
    test [ "astLiteralTest" ~: astLiteralTest 
         , "astPrimitiveTest" ~: astPrimitiveTest
         , "astLetTest" ~: astLetTest
         ]
