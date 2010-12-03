module MicroScheme.ParserTests (parserTests) where

import Test.HUnit

import MicroScheme.Value
import MicroScheme.Parser

-- |Assert that we expect 'input' to parse as 'expected'.
assertParse expected input =
    case parseScheme input of
      Left parseError -> assertFailure (show parseError)
      Right ast -> assertEqual description expected ast
  where description = "parse of " ++ show input

parseIntTest   = assertParse (Literal (IntValue 1))      " 1 "
parseFloatTest = assertParse (Literal (FloatValue 2.5))  " 2.5 "
parseTrueTest  = assertParse (Literal (BoolValue True))  " #t "
parseFalseTest = assertParse (Literal (BoolValue False)) " #f "

parseEmptyListTest = assertParse (List []) " ( ) "
parseListTest = assertParse expected " ( 1 2.0 ) "
    where expected = (List [Literal (IntValue 1),
                            Literal (FloatValue 2.0)])
parseBracketedListTest = assertParse (List []) "[]"

parseSymbolTest = assertParse (Symbol "foo!") " foo! "

parserTests =
    test [ "parseIntTest"   ~: parseIntTest
         , "parseFloatTest" ~: parseFloatTest
         , "parseTrueTest"  ~: parseTrueTest
         , "parseFalseTest" ~: parseFalseTest
         , "parseEmptyListTest" ~: parseEmptyListTest
         , "parseListTest"  ~: parseListTest
         , "parseBracketedListTest" ~: parseBracketedListTest
         , "parseSymbolTest" ~: parseSymbolTest
         ]

