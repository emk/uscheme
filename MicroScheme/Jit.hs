module MicroScheme.Jit (jit) where

import Data.Int
import Data.Word
import LLVM.Core hiding (Value)
import LLVM.ExecutionEngine

import MicroScheme.Ast
import MicroScheme.Value

mReturn1 :: CodeGenModule (Function (IO Int32))
mReturn1 =
    createFunction ExternalLinkage $ do
      ret (1::Int32)

jit :: Ast -> IO Value
jit ast = do
  return1 <- simpleFunction mReturn1
  value <- return1
  return (IntValue (toInteger value))
