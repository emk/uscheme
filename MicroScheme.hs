module MicroScheme
    (Value(IntValue, FloatValue, BoolValue),
     Ast(Literal, List, Symbol),
     parseScheme) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as P

-- |Our MicroScheme dialect supports only 3 data types.  Note that unlike
-- a traditional Scheme, MicroScheme can't represent parsed source code
-- as literal data.
data Value = IntValue Integer
           | FloatValue Double
           | BoolValue Bool
  deriving (Eq, Show)

-- |A node in the abstract syntax tree of MicroScheme source.
data Ast = Literal Value
         | List [Ast]
         | Symbol String
  deriving (Eq, Show)

-- Legal identifier start characters in Scheme.
schemeIdentStart = letter <|> oneOf "!$%&*/:<=>?^_~"

-- Legal identifier characters in Scheme.
schemeIdentLetter = schemeIdentStart <|> digit <|> oneOf "+-.@"

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
  asts <- many sexp
  lexeme (char end)
  return $ List asts

-- A regular Scheme list, or a Racket-style list with [].
list = listWithDelimiters '(' ')' <|> listWithDelimiters '[' ']' <?> "list"

-- A Scheme symbol.
symbol = fmap Symbol identifier <?> "symbol"

-- S-expressions.
sexp = literal <|> list <|> symbol

-- |Parse a Scheme expression into an 'Ast'.
parseScheme :: String -> Either ParseError Ast
parseScheme str = parse parser "<input>" str
  where parser = do
          whiteSpace
          ast <- sexp
          eof
          return ast
