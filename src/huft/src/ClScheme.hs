module ClScheme where

-- This file defines Closed Scheme, 
-- which is the target language of closure conversion. 
-- Closed Scheme is just First-Order Scheme extended with CLOSURE, 
-- CAPTURED, and LETREC constructs. 
-- When you receive the file, 
-- it will have everything you need except a couple of cases of 
-- embedding of Closed Scheme into vScheme.
-- You’ll write these cases in step (@embedcases).

import qualified ObjectCode as O
import qualified Primitives as P
import qualified Case
import qualified Constructed


type Name = String
-- funcode, captured variable
data Closure = Closure [Name] Exp [Exp] deriving Show
-- invariant: has no free (Local) variables
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
  -- module 12
  | Constructed (Constructed.T Exp)
  | Case (Case.T Exp)
  deriving Show

data Def = Val Name Exp
          | Define Name FunCode
          | Exp Exp
          | CheckExpect String Exp String Exp
          | CheckAssert String Exp