module CodeGen where

import qualified ObjectCode as O
import qualified Asm as A
import qualified KNF as K


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
toReg' dest e = undefined

forEffect' :: K.Exp Reg -> HughesList Instruction
forEffect' e = undefined

toReturn' :: K.Exp Reg -> HughesList Instruction
toReturn' e = undefined

-- forEffect cannot fail
forEffect :: K.Exp Reg -> [Instruction]
forEffect e = forEffect' e []
