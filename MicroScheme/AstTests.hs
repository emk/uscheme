module MicroScheme.AstTests (astTests) where

import Test.HUnit

import MicroScheme.Ast
import MicroScheme.Parser
import MicroScheme.Value

assertAst expected input = do
  sexp <- failOnParseError (parseSexp "<test case>" input)
  assertEqual ("building ast for " ++ show input) expected (buildAst sexp)

astLiteralTest = assertAst (Literal (IntValue 1)) "1"

astTests =
    test [ "astLiteralTest" ~: astLiteralTest 
         ]
