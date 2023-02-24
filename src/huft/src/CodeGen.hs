module CodeGen where

import qualified ObjectCode as O
import qualified Asm as A
import qualified KNF as K
import qualified Primitives as P


type Reg = O.Reg
type Instruction = A.Instr

----- Join lists, John Hughes (1985) style -----

type HughesList a = [a] -> [a]

empty :: HughesList a
empty tail = tail

-- singleton
s :: a -> HughesList a
s e tail = e : tail

-- conversion
l :: [a] -> HughesList a
l es tail = es ++ tail

---- the code generator ----

toReg' :: Reg -> K.Exp Reg -> HughesList Instruction
toReg' dest e = case e of
  K.Literal lit -> s (A.ObjectCode (O.RegLit "loadliteral" dest lit))
  _ -> error "implementme"

forEffect' :: K.Exp Reg -> HughesList Instruction
forEffect' e = case e of
  K.VMOP (P.HasEffect (P.Base op _)) args -> s (A.ObjectCode (O.Regs op args))
  K.Let x e e' -> toReg' x e . forEffect' e'

toReturn' :: K.Exp Reg -> HughesList Instruction
toReturn' e = undefined

-- forEffect cannot fail
forEffect :: K.Exp Reg -> [Instruction]
forEffect e = forEffect' e []
