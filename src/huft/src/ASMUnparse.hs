module ASMUnparse where

import qualified ASM as A
import qualified ObjectCode as O

reg :: Show a => a -> [Char]
reg r = "r" ++ show r

unparseLit :: O.Literal -> String
unparseLit = error "not implemented"

unparse1 :: A.Instr -> String
unparse1 i = case i of
  A.ObjectCode (O.Regs op regs) -> unwords $ op : map reg regs
  A.DefLabel label -> unwords ["def", label]
  A.GotoLabel label -> unwords ["goto", label]
  A.IfGotoLabel r label -> unwords ["if", reg r, "goto", label]
  _ -> error "unkown assembly instruction"

unparse :: [A.Instr] -> [String]
unparse (i:is) =
  case i of
    A.LoadFunc r arity body -> ".loadfunc" : unparse body ++
                               ".endload" : unparse is
    _ -> unparse1 i : unparse is
unparse [] = []