-- |Parse MicroScheme source code into an 'Sexp'.  This module requires
-- some knowledge of Haskell and Parsec, and may be safely treated as a
-- a black box if you're not interested in the details.
module MicroScheme.Parser (parseSexp, parseSexps, failOnParseError) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as P

import MicroScheme.Value

-- Legal identifier start characters in Scheme, plus "+-@", which have
-- been moved from 'schemeIdentLetter' for simplicity and compatibility
-- with PLT.
schemeIdentStart = letter <|> oneOf "!$%&*/:<=>?^_~+-@"

-- Legal identifier characters in Scheme.
schemeIdentLetter = schemeIdentStart <|> digit <|> char '.'

-- Syntactic properties of the Scheme language.
schemeStyle =
    LanguageDef { commentStart = "#|"
                , commentEnd = "|#"
                , commentLine = ";"
                , nestedComments = True
                , identStart = schemeIdentStart
                , identLetter = schemeIdentLetter
                , opStart = oneOf ""
                , opLetter = oneOf ""
                , reservedNames = []
                , reservedOpNames = []
                , caseSensitive = True
                }

-- Parse tokens as defined by 'schemeStyle'.
lexer = P.makeTokenParser schemeStyle

-- Scheme-specific tokens based on 'lexer'.
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
integerToken = lexeme (P.decimal lexer) -- Does not include lexeme? Weird.
floatToken = P.float lexer
boolToken = lexeme rawBool <?> "boolean"
    where rawBool = do
            char '#'
            l <- oneOf "tf"
            return $ l == 't'

-- Literal values.
int = fmap IntValue integerToken
float = fmap FloatValue floatToken
bool = fmap BoolValue boolToken
literal = fmap Literal $ try float <|> int <|> bool

-- Parse a Scheme list with the specified delimiters.
listWithDelimiters begin end = do
  lexeme (char begin)
  sexps <- many sexp
  lexeme (char end)
  return $ List sexps

-- A regular Scheme list, or a Racket-style list with [].
list = listWithDelimiters '(' ')' <|> listWithDelimiters '[' ']' <?> "list"

-- A Scheme symbol.
symbol = fmap Symbol identifier <?> "symbol"

-- S-expressions.
sexp = literal <|> list <|> symbol

-- Parse source 'input' using 'parser'.
parseSource parser inputName input = parse wrappedParser inputName input
    where wrappedParser = do
            whiteSpace
            result <- parser
            eof
            return result

-- |Parse a Scheme expression into an 'Sexp'.
parseSexp :: String -> String -> Either ParseError Sexp
parseSexp inputName str = parseSource sexp inputName str

-- |Parse multiple s-expressions, such those at the top level of a file.
parseSexps :: String -> String -> Either ParseError [Sexp]
parseSexps inputName str = parseSource (many sexp) inputName str

-- |If a parse error has occurred, call 'fail' in the current monad.
failOnParseError :: (Monad m) => Either ParseError a -> m a
failOnParseError (Left err)  = fail (show err)
failOnParseError (Right val) = return val
