-- |Analyze Sexp objects and convert them into more rigorously-defined
-- abstract syntax trees for use by later stages.
module MicroScheme.Ast (Ast(LiteralNode), buildAst) where

import MicroScheme.Value

data Ast = LiteralNode Value
         | Primitive String Ast Ast
         | Let [(String, Ast)] Ast
         | Body [Ast]
  deriving (Eq, Show)

buildAst :: Sexp -> Ast
buildAst sexp = LiteralNode (IntValue 1)