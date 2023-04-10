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
           | If a (Exp a) (Exp a)
           | Let a (Exp a) (Exp a)
           | LetRec [(a, Closure a)] (Exp a)
           | Seq (Exp a) (Exp a)
           | Assign a (Exp a)
           | While a (Exp a) (Exp a)
           | FunCode [a] (Exp a)
           | FunCall a [a]
           | VMOP VMOP [a]
           | VMOPGLO VMOP [a] Literal
           | Captured Int
           | ClosureX (Closure a)
           deriving Show

type Funcode a = ([a], Exp a) -- lambda with no free names

data Closure a = Closure [a] (Exp a) [a] -- funcode, registers holding values of captured variables
    deriving Show

--    create these @(x,...x, v) forms:
--      setglobal(register, name-of-global)
--      getglobal(name-of-global)

--    you could consider adding similar functions for `check`, `expect`,
--    and `check-assert`

setglobal:: String -> a -> Exp a
setglobal x register = VMOPGLO P.setglobal [register] (O.String x)

getglobal:: String -> Exp a
getglobal x = VMOPGLO P.getglobal [] (O.String x)

