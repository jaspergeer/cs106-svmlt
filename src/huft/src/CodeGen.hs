module CodeGen where

import qualified ObjectCode as O
import qualified Asm as A
import qualified KNF as K
import qualified Primitives as P
import qualified AsmUtils as U
import Control.Monad.Trans.State (state, runState, evalState)


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

-- I am very proud of this
(<.>) :: (Applicative f) => f (b -> c) -> f (a -> b) -> f (a -> c)
x <.> y = (.) <$> x <*> y

---- the code generator ----

toReg' :: Reg -> K.Exp Reg -> U.UniqueLabelState (HughesList Instruction)
toReg' dest e = case e of
    K.Literal lit -> return $ s (A.ObjectCode (O.RegLit "loadliteral" dest lit))
    _ -> error (show e)

forEffect' :: K.Exp Reg -> U.UniqueLabelState (HughesList Instruction)
forEffect' e = case e of
  K.VMOP (P.HasEffect (P.Base op _)) args -> return $ s (A.ObjectCode (O.Regs op args))
  K.Let x e e' -> toReg' x e <.> forEffect' e'

toReturn' :: K.Exp Reg -> U.UniqueLabelState (HughesList Instruction)
toReturn' e = undefined

forEffect :: K.Exp Reg -> [Instruction]
forEffect e = evalState (forEffect' e) 0 []
