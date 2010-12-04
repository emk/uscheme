module MicroScheme.Jit (jit) where

import Data.Int
import Data.Word
import qualified LLVM.Core as L
import qualified LLVM.ExecutionEngine as L

import MicroScheme.Ast
import MicroScheme.Value

mReturn1 :: L.CodeGenModule (L.Function (IO Int32))
mReturn1 =
    L.createFunction L.ExternalLinkage $ do
      L.ret (1::Int32)

jit :: Ast -> IO Value
jit ast = do
  return1 <- L.simpleFunction mReturn1
  value <- return1
  return (IntValue (toInteger value))
