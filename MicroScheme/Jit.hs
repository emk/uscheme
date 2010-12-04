module MicroScheme.Jit (jit) where

import qualified LLVM.Core as L
import qualified LLVM.ExecutionEngine as L

import MicroScheme.Ast
import MicroScheme.Value

jit :: Ast -> IO Value
jit ast = return (IntValue 1)
