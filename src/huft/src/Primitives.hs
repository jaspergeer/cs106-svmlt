module Primitives where

type Name = String

data Base = Base Name Int

data Primitive = SetsRegister Base
               | HasEffect Base


-- (* For SETS_REGISTER, the instruction takes the destination register
--    as the first operand, and the actual parameters as remaining operands.

--    For HAS_FFECT, the instruction has no destination register;
--    the operands are the arguments.
    
--    A SETS_REGISTER primitive _must not_ have a side effect,
--    because if one appears in an effectful context, it is discarded.
--  *)

arity :: Primitive -> Int
-- need to be implemented
arity _ = 1

primitives = []

find x = error "implementme"