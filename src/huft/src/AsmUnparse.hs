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

unparseObj1 :: O.Instr -> String
unparseObj1 i = case i of
  (O.Regs "zero" [r1]) -> unwords [reg r1, ":=", "0"]
  (O.Regs op regs) -> case regs of
    [] -> op
    [r1] -> unwords [op, reg r1]
    [r1, r2] | op `elem` A.unops -> unwords [reg r1, ":=", op, reg r2]
    [r1, r2] -> unwords [op, reg r1, reg r2]
    [r1, r2, r3] | op `elem` A.binops -> unwords [reg r1, ":=", reg r2, op, reg r3]
    [r1, r2, r3] -> unwords [reg r1, ":=", op, reg r2, reg r3]
  (O.RegLit "loadliteral" r1 lit) -> unwords [reg r1, ":=", unparseLit lit]
  (O.RegLit "popen" r1 lit) -> unwords [reg r1, ":= popen", unparseLit lit]
  (O.RegLit op r1 lit) -> unwords [op, reg r1, unparseLit lit]
  (O.RegGlo "getglobal" r1 name) -> unwords [reg r1, ":= G[" ++ name ++ "]"]
  (O.RegGlo "setglobal" r1 name) -> unwords ["G[" ++ name ++ "] :=", reg r1]
  (O.RegsInt op [] i24) -> unwords [op, show i24]
  (O.RegsInt op [r1] u16) -> unwords [reg r1, ":=", op, show u16]
  (O.RegsInt "mkclosure" [r1, r2] u8) -> unwords [reg r1, ":= closure["++reg r2 ++ "," ++ show u8 ++ "]"]
  (O.RegsInt "getclslot" [r1, r2] u8) -> unwords [reg r1, ":=", reg r2 ++ "." ++ show u8]
  (O.RegsInt "setclslot" [r1, r2] u8) -> unwords [reg r1 ++ "."++ show u8, ":=", reg r2]
  (O.RegsInt "mkblock"   [r1, r2] u8) -> unwords [reg r1, ":= block[" ++ reg r2 ++ "," ++ show u8 ++ "]"]
  (O.RegsInt "setblkslot" [r1, r2] u8) -> unwords ["block", reg r1 ++ "."++ show u8, ":=", reg r2]
  (O.RegsInt "getblkslot" [r1, r2] u8) -> unwords [reg r1, ":=", "block", reg r2 ++ "."++ show u8]
  -- _ -> show i

unparse1 :: A.Instr -> String
unparse1 (A.ObjectCode instr) = unparseObj1 instr
unparse1 i = case i of
  A.DefLabel label -> unwords ["def", label]
  A.GotoLabel label -> unwords ["goto", label]
  A.IfGotoLabel r label -> unwords ["if", reg r, "goto", label]
  A.GotoVCon r choices ->
    let choice (v, arity, lbl) = unwords ["  case", unparseLit v, "(" ++ show arity ++ "):", "goto", lbl]
            in unwords ["switch", reg r, "{", unwords (map choice choices), "}"]
            -- : map choice choices ++ ["}"] ++ unparse instructions
  _ -> error "IMPOSSIBLE: unknown assembly instruction"

-- | unparse (A.GOTO_VCON (r, choices) :: instructions) =
--     let fun choice (v, arity, lbl) =
--            spaceSep ["  case", lit v, "(" ^ int arity ^ "):", "goto", lbl]
--     in  spaceSep ["switch", reg r, "{"] :: map choice choices @ "}" ::
--         unparse instructions
--     end

unparse :: [A.Instr] -> [String]
unparse (i:is) = case i of
    A.LoadFunc r arity body -> (reg r ++ " := fun " ++ show arity ++ " {") : map ("\t" ++) (unparse body) ++
                               "}" : unparse is
    A.ObjectCode (O.LoadFunc r arity body) ->
      unparse (A.LoadFunc r arity (map A.ObjectCode body):is)
    _ -> unparse1 i : unparse is
unparse [] = []