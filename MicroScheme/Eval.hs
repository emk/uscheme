module MicroScheme.Eval (eval) where

import qualified Data.Map as M

import MicroScheme.Value

type Environment = M.Map String Value

defaultEnv :: Environment
defaultEnv = M.empty

evalBody env (expr:[]) = evalExpr env expr
evalBody env (expr:exprs) = do
  evalExpr env expr
  evalBody env exprs

evalLet env bindings body = do
  evalBody newEnv body
    where newEnv = M.insert "x" (IntValue 1) env

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
primCall "+" (x:y:[]) = return (arithmeticBinOp "+" (+) (+) x y)
primCall name args =
  error ("Don't know how to call " ++ name ++ " with " ++ show args)

-- Evaluate a Scheme expression in 'env'.
evalExpr :: Environment -> Ast -> IO Value
evalExpr env (Literal value) = return value
evalExpr env (List ((Symbol "let"):bindings:body)) = do
  evalLet env bindings body
evalExpr env (List ((Symbol name):args)) = do
  args' <- mapM (evalExpr env) args
  primCall name args'
evalExpr env (Symbol name) =
    case M.lookup name env of
      Just val -> return val
      Nothing  -> error ("Unbound variable: " ++ name)
evalExpr env ast = error ("Don't know how to eval " ++ show ast)

-- |Evaluate a Scheme expression.
eval :: Ast -> IO Value
eval ast = evalExpr defaultEnv ast
