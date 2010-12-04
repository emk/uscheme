module MicroScheme.Jit (jit) where

import Data.Int
import Data.Word
import LLVM.Core hiding (Value)
import LLVM.ExecutionEngine

import MicroScheme.Ast
import MicroScheme.Value

mReturnInt :: Int32 -> CodeGenModule (Function (IO Int32))
mReturnInt i =
    createFunction ExternalLinkage $ do
      ret i

jit :: Ast -> IO Value

jit (Literal (IntValue i)) = do
  code <- simpleFunction (mReturnInt (fromInteger i))
  value <- code
  return (IntValue (toInteger value))

jit ast = error ("Don't know how to JIT " ++ show ast)