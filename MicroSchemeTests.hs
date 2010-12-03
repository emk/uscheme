module Main where

import Test.HUnit

import MicroScheme

-- |Assert that we expect 'input' to parse as 'expected'.
assertParse expected input =
    case parseScheme input of
      Left parseError -> assertFailure (show parseError)
      Right ast -> assertEqual description expected ast
  where description = "parse of " ++ show input

parseIntTest   = assertParse (Literal (IntValue 1))      "1"
parseFloatTest = assertParse (Literal (FloatValue 2.5))  "2.5"
parseTrueTest  = assertParse (Literal (BoolValue True))  "#t"
parseFalseTest = assertParse (Literal (BoolValue False)) "#f"

tests = test [ "parseIntTest"   ~: parseIntTest
             , "parseFloatTest" ~: parseFloatTest
             , "parseTrueTest"  ~: parseTrueTest
             , "parseFalseTest" ~: parseFalseTest
             ]

main = runTestTT $ tests

