module ASMUnparse where

import qualified ASM as A
import qualified ObjectCode as O
import qualified Data.Set as S

reg :: Show a => a -> [Char]
reg r = "r" ++ show r

unparseLit :: O.Literal -> String
unparseLit = error "not implemented"

binopSet = S.fromList A.binops

unparse1 :: A.Instr -> String
unparse1 (A.ObjectCode (O.Regs op [])) = op
unparse1 (A.ObjectCode (O.Regs op [r1])) = unwords [op, reg r1]
unparse1 (A.ObjectCode (O.Regs op (r1:r2:r3))) = if S.member op binopSet
  then unwords [reg r1, ":=", reg r2, op, reg r3]
  else unwords ([reg r1, ":="] ++ map reg (r2:r3))
unparse1 i = case i of
  A.DefLabel label -> unwords ["def", label]
  A.GotoLabel label -> unwords ["goto", label]
  A.IfGotoLabel r label -> unwords ["if", reg r, "goto", label]
  _ -> error "IMPOSSIBLE: unknown assembly instruction"

unparse :: [A.Instr] -> [String]
unparse (i:is) =
  case i of
    A.LoadFunc r arity body -> ".loadfunc" : unparse body ++
                               ".endload" : unparse is
    _ -> unparse1 i : unparse is
unparse [] = []