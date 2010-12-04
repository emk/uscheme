module MicroScheme.Eval (evalInDefaultEnv) where

import qualified Data.Map as M

import MicroScheme.Ast
import MicroScheme.Value

type Environment = M.Map String Value

defaultEnv :: Environment
defaultEnv = M.empty

envLookup = M.lookup

envInsertMany :: [(String, Value)] -> Environment -> Environment
envInsertMany [] env = env
envInsertMany ((s,v):svs) env = envInsertMany svs (M.insert s v env)

evalBinding env (name,ast) = do
  value <- eval env ast
  return (name, value)

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
eval :: Environment -> Ast -> IO Value

eval env (Literal value) = return value

eval env (Var name) = 
    case envLookup name env of
      Just val -> return val
      Nothing  -> error ("Unbound variable: " ++ name)

eval env (Primitive name args) = do
  args' <- mapM (eval env) args
  primCall name args'

eval env (Let bindings body) = do
  bindings' <- mapM (evalBinding env) bindings
  eval (envInsertMany bindings' env) body

eval env (Body (expr:[])) = eval env expr
eval env (Body (expr:exprs)) = do
  eval env expr
  eval env (Body exprs)

eval env ast = error ("Don't know how to eval " ++ show ast)

-- |Evaluate a Scheme expression in the default environment.
evalInDefaultEnv :: Ast -> IO Value
evalInDefaultEnv ast = eval defaultEnv ast
