-- |Analyze Sexp objects and convert them into more rigorously-defined
-- abstract syntax trees for use by later stages.
module MicroScheme.Ast (Ast(Literal), buildAst) where

import MicroScheme.Value

data Ast = Literal Value
         | Primitive String Ast Ast
         | Let [(String, Ast)] Ast
         | Body [Ast]
  deriving (Eq, Show)

buildAst :: Sexp -> Ast
buildAst sexp = Literal (IntValue 1)