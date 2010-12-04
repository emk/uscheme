-- |The heart of system: Scheme values, including compile-time 'Value'
-- values that we don't try to implement in the runtime system.
module MicroScheme.Value
    (Value(IntValue, FloatValue, BoolValue),
     Sexp(RuntimeValue, List, Symbol)) where

-- |Our MicroScheme dialect supports only 3 data types.  Note that unlike
-- a traditional Scheme, MicroScheme can't represent parsed source code
-- as literal data.
data Value = IntValue Integer
           | FloatValue Double
           | BoolValue Bool
  deriving (Eq, Show)

-- |A compile-time Scheme expression.  In a normal Scheme, lists, symbols
-- and other data types would also exist at compile time.  But in
-- MicroScheme, we're trying to keep the number of runtime types very
-- small for the sake of implementation simplicity.
data Sexp = RuntimeValue Value
          | List [Sexp]
          | Symbol String
  deriving (Eq, Show)
