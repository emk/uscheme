module MicroScheme.Eval (eval) where

import MicroScheme.Value

-- Evaluate a binary arithmetic operation, promoting our arguments so that
-- the types match.
arithmeticBinOp :: String ->
                   (Integer -> Integer -> Integer) -> 
                   (Double -> Double -> Double) ->
                   Value -> Value -> Value
arithmeticBinOp name intOp floatOp x y = op x y
  where
    op (IntValue x)   (IntValue y)   = IntValue   (intOp x y)
    op (IntValue x)   (FloatValue y) = FloatValue (floatOp (fromInteger x) y)
    op (FloatValue x) (IntValue y)   = FloatValue (floatOp x (fromInteger y))
    op (FloatValue x) (FloatValue y) = FloatValue (floatOp x y)
    op x y = error ("Don't know how to compute " ++ name ++ " " ++
                    show x ++ " " ++ show y)

-- Evaluate a function call.
evalCall "+" (x:y:[]) = return (arithmeticBinOp "+" (+) (+) x y)
evalCall name args =
  error ("Don't know how to call " ++ name ++ " with " ++ show args)

-- |Evaluate a Scheme expression.
eval :: Ast -> IO Value
eval (Literal value) = return value
eval (List ((Symbol name):args)) = do
  args' <- mapM eval args
  evalCall name args'
eval ast = error ("Don't know how to eval " ++ show ast)
