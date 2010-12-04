-- |Analyze Sexp objects and convert them into more rigorously-defined
-- abstract syntax trees for use by later stages.
module MicroScheme.Ast (Ast(Literal), buildAst) where

import MicroScheme.Value

-- |An abstract syntax tree, which is built from raw 'Sexp' objects and
-- consumed by latter stages of interpreter or compilation.
data Ast = Literal Value
         | Primitive String [Ast]
         | Let [(String, Ast)] Ast
         | Body [Ast]
  deriving (Eq, Show)

-- |Construct an Ast from an Sexp.
buildAst :: Sexp -> Ast
buildAst (RuntimeValue v) = Literal v
buildAst ast = error ("Unable to build Ast for " ++ show ast)
