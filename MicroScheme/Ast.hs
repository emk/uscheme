-- |Analyze Sexp objects and convert them into more rigorously-defined
-- abstract syntax trees for use by later stages.
module MicroScheme.Ast (Ast(Literal, Var, Primitive, Let, Body), buildAst) where

import MicroScheme.Value

-- |An abstract syntax tree, which is built from raw 'Sexp' objects and
-- consumed by latter stages of interpretation or compilation.
data Ast = Literal Value
         | Var String
         | Primitive String [Ast]
         | Let [(String, Ast)] Ast
         | Body [Ast]
  deriving (Eq, Show)

-- Build an abstract syntax tree for a 'let' binding.
buildAstForBinding (List ((Symbol name):sexp:[])) = (name, buildAst sexp)
buildAstForBinding binding = error ("Can't parse binding " ++ show binding)

-- |Construct an Ast from an Sexp.  This is a lot of messy pattern matching,
-- so we try to isolate it all in one module.
buildAst :: Sexp -> Ast

buildAst (RuntimeValue v) = Literal v

buildAst (List ((Symbol "let"):(List bindings):body)) =
    Let (map buildAstForBinding bindings) (Body (map buildAst body))

buildAst (List ((Symbol name):args)) = Primitive name (map buildAst args)

buildAst (Symbol name) = Var name

buildAst sexp = error ("Unable to build Ast for " ++ show sexp)
