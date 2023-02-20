module KNF where

-- You'll define the representation of `'a exp`, and you'll write
--    the utility functions.
import qualified ObjectCode as O
import qualified Primitives as P

type Literal = O.Literal

type VMOP = P.Primitive

-- type parameter 'a is a _name_, typically
-- instantiated as `string` or `ObjectCode.reg`

data Exp a = Literal Literal
           | Name a
           | IF a (Exp a) (Exp a)
           | Let a [(a, Exp a)] (Exp a)
           | Seq (Exp a) (Exp a)
           | Assign a (Exp a)
           | While a (Exp a) (Exp a)
           | FunCode [a] (Exp a)
           | VMOP VMOP [a]
           | VMOPGLO VMOP [a] Literal