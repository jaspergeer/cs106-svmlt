module ClScheme where

import qualified ObjectCode as O
import qualified Primitives as P

type Name = String
data Closure = Closure [Name] Exp [Exp]
data FunCode = FunCode [Name] Exp

data Exp = Literal O.Literal
  | Local Name
  | Global Name
  | IfX Exp Exp Exp
  | PrimCall P.Primitive [Exp]
  | FunCall Exp [Exp]
  | Let [(Name, Exp)] Exp
  | Begin [Exp]
  | SetLocal Name Exp
  | SetGlobal Name Exp
  | WhileX Exp Exp
  | Captured Int
  | ClosureX Closure
  | LetRec [(Name, Closure)] Exp

data Def = Val Name Exp
          | Define Name FunCode
          | Exp Exp
          | CheckExpect String Exp String Exp
          | CheckAssert String Exp