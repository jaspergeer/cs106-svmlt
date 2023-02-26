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
    -- forms with direct translation
    K.Literal lit -> return $ s (A.ObjectCode (O.RegLit "loadliteral" dest lit))
    K.Name a -> return $ s $ U.copyreg dest a
    K.VMOP prim [r1, r2] -> return $ s $ U.setReg dest prim [r1, r2]
    K.VMOPGLO prim [r1] lit -> return $ s $ U.setRegLit dest prim lit -- the [r1] list disappears here, is that right?
    K.FunCall r args -> return $ s $ A.ObjectCode (O.Regs "call" (r:args)) -- provided x, x1, ... xn, are consecutive
    K.FunCode args body -> do
        b' <- toReturn' body
        return $ s $ A.LoadFunc dest (length args) (b' [])
    -- control flow forms
    K.If x e1 e2 -> do
      l <- U.newLabel
      l' <- U.newLabel
      branch2 <- toReg' dest e2
      branch1 <- toReg' dest e1
      return $ s (U.ifgoto x l) . branch2 . s (U.goto l') .
               s (U.deflabel l) . branch1 . s (U.deflabel l')
    K.While x e1 e2 -> forEffect' (K.While x e1 e2) <.> toReg' dest (K.Literal $ O.Bool False)
    -- floatable forms
    K.Let x e1 e' -> toReg' x e1 <.> toReg' dest e'
    K.Seq e1 e2 -> forEffect' e1 <.> toReg' dest e2 -- not sure how does this works
    K.Assign x e -> toReg' x e <.> toReg' dest (K.Name x) -- I assume this is copy reg
    _ -> error (show e) -- this must not fail in fact
    

forEffect' :: K.Exp Reg -> U.UniqueLabelState (HughesList Instruction)
forEffect' e = case e of
  K.Literal lit -> return empty
  K.Name a -> return empty
  -- assume op has side effect
  K.VMOP (P.HasEffect (P.Base op _)) args -> return $ s (A.ObjectCode (O.Regs op args))
  -- otherwise
  K.VMOP _ _ -> return empty
  K.VMOPGLO (P.HasEffect (P.Base op _)) args v -> return $ s $ 
  {- assume args only have one argument -} A.ObjectCode (O.RegLit op (head args) v)
  K.VMOPGLO {} -> return empty
  K.FunCall r args -> return $ s $ A.ObjectCode (O.Regs "call" (r:args))
  K.FunCode args body -> return empty
  K.If x e1 e2 -> do
      l <- U.newLabel
      l' <- U.newLabel
      branch2 <- forEffect' e2
      branch1 <- forEffect' e1
      return $ s (U.ifgoto x l) . branch2 . s (U.goto l') .
              s (U.deflabel l) . branch1 . s (U.deflabel l')
  K.Let x e e' -> toReg' x e <.> forEffect' e'
  K.Seq e1 e2 -> forEffect' e1 <.> forEffect' e2
  K.Assign x e -> toReg' x e
  _ -> error (show e) -- this must not fail in fact


-- wont be implementing till module 8
toReturn' :: K.Exp Reg -> U.UniqueLabelState (HughesList Instruction)
toReturn' e = undefined

forEffect :: K.Exp Reg -> [Instruction]
forEffect e = evalState (forEffect' e) 0 []
