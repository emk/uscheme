-- |The heart of system: Scheme values, including compile-time 'Ast'
-- values that we don't try to implement in the runtime system.
module MicroScheme.Value
    (Value(IntValue, FloatValue, BoolValue),
     Ast(Literal, List, Symbol)) where

-- |Our MicroScheme dialect supports only 3 data types.  Note that unlike
-- a traditional Scheme, MicroScheme can't represent parsed source code
-- as literal data.
data Value = IntValue Integer
           | FloatValue Double
           | BoolValue Bool
  deriving (Eq, Show)

-- |A node in the abstract syntax tree of MicroScheme source.  In a normal
-- Scheme, these would be moved into 'Value'.  But since none of these
-- types exist at runtime, we treat them specially.
data Ast = Literal Value
         | List [Ast]
         | Symbol String
  deriving (Eq, Show)
