module AsmUnparse where

import qualified Asm as A
import qualified ObjectCode as O
import qualified Data.Set as S

reg :: Show a => a -> [Char]
reg r = "r" ++ show r

unparseLit :: O.Literal -> String
unparseLit lit = case lit of
  O.Int n -> show n
  O.Real n -> show n
  O.String s -> '\"' : s ++ "\""
  O.Bool b -> if b then "#t" else "#f"
  O.EmptyList -> "'()"
  O.Nil -> "nil"

binopSet :: S.Set String
binopSet = S.fromList A.binops

unparseObj1 :: O.Instr -> String
unparseObj1 (O.Regs "zero" [r1]) = unwords [reg r1, ":=", "0"]
unparseObj1 (O.Regs op regs) = case regs of
  [] -> op
  [r1] -> unwords [op, reg r1]
  (r1:r2:r3) -> if S.member op binopSet
    then unwords [reg r1, ":=", reg r2, op, reg r3]
    else unwords ([reg r1, ":="] ++ map reg (r2:r3))
unparseObj1 (O.RegLit "loadliteral" r1 lit) = unwords [reg r1, ":=", unparseLit lit]
unparseObj1 (O.RegLit op r1 lit) = unwords [reg r1, ":=", op, unparseLit lit]
unparseObj1 (O.RegGlo "getglobal" r1 name) = unwords [reg r1, ":= G[", name, "]"]
unparseObj1 (O.RegGlo "setglobal" r1 name) = unwords ["G[", name, "] :=", reg r1]
-- unparseObj1 (O.RegInt op r1 n) = unwords [reg r1, show n]

unparse1 :: A.Instr -> String
unparse1 (A.ObjectCode instr) = unparseObj1 instr
unparse1 i = case i of
  A.DefLabel label -> unwords ["def", label]
  A.GotoLabel label -> unwords ["goto", label]
  A.IfGotoLabel r label -> unwords ["if", reg r, "goto", label]
  _ -> error "IMPOSSIBLE: unknown assembly instruction"

unparse :: [A.Instr] -> [String]
unparse (i:is) = case i of
    A.LoadFunc r arity body -> ".loadfunc" : map ("\t" ++) (unparse body) ++
                               ".endload" : unparse is
    A.ObjectCode (O.LoadFunc r arity body) ->
      unparse (A.LoadFunc r arity (map A.ObjectCode body):is)
    _ -> unparse1 i : unparse is
unparse [] = []