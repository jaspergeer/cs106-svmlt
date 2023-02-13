module AsmUnparse where

import qualified Asm as A
import qualified ObjectCode as O
import qualified Data.Set as S

reg :: Show a => a -> [Char]
reg r = "$r" ++ show r

unparseString :: String -> String
unparseString s = '\"' : f s ++ "\""
  where f s = case s of
          '\a' : cs -> "\\a" ++ f cs
          '\b' : cs -> "\\b" ++ f cs
          '\t' : cs -> "\\t" ++ f cs
          '\n' : cs -> "\\n" ++ f cs
          '\r' : cs -> "\\r" ++ f cs
          '\"' : cs -> "\\\"" ++ f cs
          '\\' : cs -> "\\\\" ++ f cs
          c : cs -> c : f cs
          [] -> []

unparseLit :: O.Literal -> String
unparseLit lit = case lit of
  O.Int n -> show n
  O.Real n -> show n
  O.String s -> unparseString s
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
  (r1:regs) -> if S.member op binopSet
    then case regs of
      [r2, r3] -> unwords [reg r1, ":=", reg r2, op, reg r3]
      _ -> error "IMPOSSIBLE: malformed binop instruction"
    -- else unwords ([reg r1, ":="] ++ map reg regs) waht does this line mean, does not correpoind to eR2
    else unwords ([reg r1, ":=", op] ++ map reg regs) -- I think the else branch only has R1, but correct me if I'm wrong
unparseObj1 (O.RegLit "loadliteral" r1 lit) = unwords [reg r1, ":=", unparseLit lit]
unparseObj1 (O.RegLit "popen" r1 lit) = unwords [reg r1, ":= popen", unparseLit lit]
unparseObj1 (O.RegLit op r1 lit) = unwords [op, reg r1, unparseLit lit]
unparseObj1 (O.RegGlo "getglobal" r1 name) = unwords [reg r1, ":=", name]
unparseObj1 (O.RegGlo "setglobal" r1 name) = unwords [name, ":=", reg r1]
unparseObj1 (O.RegsInt op [] i24) = unwords [op, show i24]
unparseObj1 (O.RegsInt op [r1] u16) = unwords [reg r1, ":=", op, show u16]
unparseObj1 (O.RegsInt op [r1, r2] u8) = unwords [reg r1, ":=", op, reg r2, show u8]

unparse1 :: A.Instr -> String
unparse1 (A.ObjectCode instr) = unparseObj1 instr
unparse1 i = case i of
  A.DefLabel label -> unwords ["def", label]
  A.GotoLabel label -> unwords ["goto", label]
  A.IfGotoLabel r label -> unwords ["if", reg r, "goto", label]
  _ -> error "IMPOSSIBLE: unknown assembly instruction"

unparse :: [A.Instr] -> [String]
unparse (i:is) = case i of
    A.LoadFunc r arity body -> (reg r ++ " := fun " ++ show arity ++ " {") : map ("\t" ++) (unparse body) ++
                               "}" : unparse is
    A.ObjectCode (O.LoadFunc r arity body) ->
      unparse (A.LoadFunc r arity (map A.ObjectCode body):is)
    _ -> unparse1 i : unparse is
unparse [] = []