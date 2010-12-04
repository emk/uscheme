module MicroScheme.Eval (eval) where

import qualified Data.Map as M

import MicroScheme.Value

type Environment = M.Map String Value

defaultEnv :: Environment
defaultEnv = M.empty

envLookup = M.lookup

envInsertMany :: [(String, Value)] -> Environment -> Environment
envInsertMany [] env = env
envInsertMany ((s,v):svs) env = envInsertMany svs (M.insert s v env)

evalBody env (expr:[]) = evalExpr env expr
evalBody env (expr:exprs) = do
  evalExpr env expr
  evalBody env exprs

evalBinding env (List ((Symbol name):expr:[])) = do
  value <- evalExpr env expr
  return (name, value)

evalLet env bindings body = do
  bindings' <- mapM (evalBinding env) bindings
  evalBody (envInsertMany bindings' env) body

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

-- Evaluate a primitive function call.
primCall "+" (x:y:[]) = return (arithmeticBinOp "+" (+) (+) x y)
primCall name args =
  error ("Don't know how to call " ++ name ++ " with " ++ show args)

-- Evaluate a Scheme expression in 'env'.
evalExpr :: Environment -> Sexp -> IO Value
evalExpr env (RuntimeValue value) = return value
evalExpr env (List ((Symbol "let"):(List bindings):body)) = do
  evalLet env bindings body
evalExpr env (List ((Symbol name):args)) = do
  args' <- mapM (evalExpr env) args
  primCall name args'
evalExpr env (Symbol name) =
    case envLookup name env of
      Just val -> return val
      Nothing  -> error ("Unbound variable: " ++ name)
evalExpr env sexp = error ("Don't know how to eval " ++ show sexp)

-- |Evaluate a Scheme expression.
eval :: Sexp -> IO Value
eval sexp = evalExpr defaultEnv sexp
