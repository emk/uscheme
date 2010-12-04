module MicroScheme.ParserTests (parserTests) where

import Test.HUnit

import MicroScheme.Value
import MicroScheme.Parser

-- Assert that we expect 'input' to parse as 'expected' using 'parser'.
assertParseWith parser expected input = do
  actual <- failOnParseError (parser "<test case>" input)
  assertEqual ("parsing " ++ show input) expected actual

assertParse = assertParseWith parseSexp

parseIntTest   = assertParse (RuntimeValue (IntValue 1))      " 1 "
parseFloatTest = assertParse (RuntimeValue (FloatValue 2.5))  " 2.5 "
parseTrueTest  = assertParse (RuntimeValue (BoolValue True))  " #t "
parseFalseTest = assertParse (RuntimeValue (BoolValue False)) " #f "

parseEmptyListTest = assertParse (List []) " ( ) "
parseListTest = assertParse expected " ( 1 2.0 ) "
    where expected = (List [RuntimeValue (IntValue 1),
                            RuntimeValue (FloatValue 2.0)])
parseBracketedListTest = assertParse (List []) "[]"

parseSymbolTest = assertParse (Symbol "foo!") " foo! "

parseTopLevel = assertParseWith parseSexps [List [], List []] " () () "

parserTests =
    test [ "parseIntTest"   ~: parseIntTest
         , "parseFloatTest" ~: parseFloatTest
         , "parseTrueTest"  ~: parseTrueTest
         , "parseFalseTest" ~: parseFalseTest
         , "parseEmptyListTest" ~: parseEmptyListTest
         , "parseListTest"  ~: parseListTest
         , "parseBracketedListTest" ~: parseBracketedListTest
         , "parseSymbolTest" ~: parseSymbolTest
         , "parseTopLevel"  ~: parseTopLevel
         ]

