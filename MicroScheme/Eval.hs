module MicroScheme.Eval (eval) where

import MicroScheme.Value

evalCall "+" (IntValue x:IntValue y:[]) = IntValue (x + y)
evalCall name args =
  error ("Don't know how to call " ++ name ++ " with " ++ show args)

-- |Evaluate a Scheme expression.
eval :: Ast -> Value
eval (Literal value) = value
eval (List ((Symbol name):args)) = evalCall name (map eval args)
eval ast = error ("Don't know how to eval " ++ show ast)
