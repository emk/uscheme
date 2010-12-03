module MicroScheme.Eval (eval) where

import MicroScheme.Value

-- |Evaluate a Scheme expression.
eval :: Ast -> Value
eval (Literal l) = l
