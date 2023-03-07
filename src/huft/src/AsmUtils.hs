module AsmUtils where

import qualified Asm
import qualified Primitives as P
import Control.Monad.Trans.State (State, get, put)
import Control.Exception (Exception, throw)
import qualified Asm as A
import qualified ObjectCode as O

type UniqueLabelState a = State Int a

-- https://stackoverflow.com/questions/6311512/creating-unique-labels-in-haskell

newLabel :: UniqueLabelState String
newLabel = do
  l <- get
  put (l + 1)
  l' <- get
  return ('L' : show l)

type Reg = O.Reg
type Instruction = Asm.Instr
type Label = String
type VMOP = P.Primitive
type Literal = O.Literal

newtype InternalError = InternalError String deriving (Show)
instance Exception InternalError

asValue :: P.Primitive -> String
asValue p = case p of
  P.HasEffect  _ -> throw (InternalError ("primtive " ++ P.name p ++ " used for value"))
  P.SetsRegister _ -> P.name p

asEffect :: P.Primitive -> String
asEffect p = case p of
  P.SetsRegister _ -> throw (InternalError ("primtive " ++ P.name p ++ " used for effect"))
  P.HasEffect  _ -> P.name p

i = A.ObjectCode

regs opr rs = i $ O.Regs opr rs

reglit opr reg lit = i $ O.RegLit opr reg lit

-- Our RegLit cnstructor only takes in one register
-- so we don't need to make it a list
setReg dest operator args = i $ O.Regs (asValue operator) (dest : args)
setRegLit dest operator v = i $ O.RegLit (asValue operator) dest v
effect operator args = i $ O.Regs (asEffect operator) args
effectLit operator args v = i $ O.RegLit (asEffect operator) args v

goto label = A.GotoLabel label
deflabel l = A.DefLabel l
ifgoto reg l = A.IfGotoLabel reg l
loadfunc r k body = A.LoadFunc r k body
loadlit r v = setRegLit r P.loadliteral v

getglobal :: O.Reg -> O.Literal -> A.Instr
getglobal dest name = setRegLit dest P.getglobal name
setglobal name reg = effectLit P.setglobal reg name

-- Implement register-register move. 
-- In file asmutil.sml, correct the implementation of copyreg.
-- Your SVM will need an opcode for an instruction that copies a value from one register to another. 
-- Just use that opcode with the internal regs function.

copyreg dest src = i $ O.Regs "copy" [dest, src]