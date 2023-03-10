module CodeGen where

import qualified ObjectCode as O
import qualified Asm as A
import qualified KNF as K
import qualified Primitives as P
import qualified AsmUtils as U
import Control.Monad.Trans.State (state, evalState)

 
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

makeIf toX x e1 e2 = do
      l <- U.newLabel
      l' <- U.newLabel
      branch2 <- toX e2
      branch1 <- toX e1
      return $ s (U.ifgoto x l) . branch2 . s (U.goto l') .
               s (U.deflabel l) . branch1 . s (U.deflabel l')

toReg' :: Reg -> K.Exp Reg -> U.UniqueLabelState (HughesList Instruction)
toReg' dest e = case e of
    -- forms with direct translation
    K.Literal lit -> return $ s $ U.reglit "loadliteral" dest lit
    K.Name a -> return $ s $ U.copyreg dest a
    K.VMOP prim@(P.SetsRegister _) rs -> return $ s $ U.setReg dest prim rs
    K.VMOP {} -> forEffect' e <.> return (s $ U.reglit "loadliteral" dest O.Nil) -- "undefined behavior"
    K.VMOPGLO prim@(P.SetsRegister _) _ lit -> return $ s $ U.setRegLit dest prim lit -- the [r1] list disappears here, is that right?
    K.VMOPGLO {} -> forEffect' e
    K.FunCall funreg args -> return $ s $ U.call dest funreg args
    K.FunCode args body -> do
        b' <- toReturn' body
        return $ s $ A.LoadFunc dest (length args) (b' [])
    -- control flow forms
    K.If x e1 e2 -> makeIf (toReg' dest) x e1 e2
    K.While x e1 e2 -> forEffect' (K.While x e1 e2) <.> toReg' dest (K.Literal $ O.Bool False)
    -- floatable forms
    K.Let x e1 e' -> toReg' x e1 <.> toReg' dest e'
    K.Seq e1 e2 -> forEffect' e1 <.> toReg' dest e2 -- not sure how does this works
    K.Assign x e -> toReg' x e <.> toReg' dest (K.Name x) -- I assume this is copy reg


forEffect' :: K.Exp Reg -> U.UniqueLabelState (HughesList Instruction)
forEffect' e = case e of
  K.Literal lit -> return empty
  K.Name a -> return empty
  -- assume op has side effect
  K.VMOP (P.HasEffect (P.Base op _)) args -> return $ s (A.ObjectCode (O.Regs op args))
  -- otherwise
  K.VMOP _ _ -> return empty
  K.VMOPGLO (P.HasEffect (P.Base op _)) args v -> return $ s $
  {- assume args only have one argument -}
  {- assume args only have one argument -} A.ObjectCode (O.RegLit op (head args) v)
  -- here we did not catch getglo because it is a SetRegister Primitive
  -- the O.RegLit is also worrysome
  K.VMOPGLO {} -> return empty
  K.FunCall funreg _ -> toReg' funreg e -- what register is killed by the call
  K.FunCode args body -> return empty
  K.If x e1 e2 -> makeIf forEffect' x e1 e2
  K.While x e e' -> do
    l <- U.newLabel
    l' <- U.newLabel
    body <- forEffect' e'
    check <- toReg' x e
    return $ s (U.goto l) . s (U.deflabel l') . body .
             s (U.deflabel l) . check . s (U.ifgoto x l')
  K.Let x e e' -> toReg' x e <.> forEffect' e'
  K.Seq e1 e2 -> forEffect' e1 <.> forEffect' e2
  K.Assign x e -> toReg' x e

-- wont be implementing till module 8
toReturn' :: K.Exp Reg -> U.UniqueLabelState (HughesList Instruction)
toReturn' e = case e of
  K.Literal lit -> toReg' 0 e <.> return (s $ U.regs "return" [0])
  K.Name a -> return $ s $ U.regs "return" [a]
  K.VMOP prim args -> toReg' 0 e <.> return (s $ U.regs "return" [0])
  K.VMOPGLO prim args v -> toReg' 0 e <.> return (s $ U.regs "return" [0])
  K.FunCall funreg args -> return $ s $ U.tailcall funreg args
  K.FunCode args body -> toReg' 0 e <.> return (s $ U.regs "return" [0])
  -- control flow
  K.If x e1 e2 -> do
      l <- U.newLabel
      branch2 <- toReturn' e2
      branch1 <- toReturn' e1
      return $ s (U.ifgoto x l) . branch2 .
               s (U.deflabel l) . branch1
  K.While x e e' -> toReg' 0 e <.> return (s $ U.regs "return" [0])
  -- floatable
  K.Seq e1 e2 -> forEffect' e1 <.> toReturn' e2
  K.Let x e e' -> toReg' x e <.> toReturn' e'
  K.Assign _ e -> toReturn' e

codeGen :: [K.Exp Reg] -> [Instruction]
codeGen es = foldr (.) empty (evalState (mapM forEffect' es) 0) []
