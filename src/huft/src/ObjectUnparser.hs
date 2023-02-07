module ObjectUnparser where
import qualified ObjectCode as O
import Data.Char (ord)
import Data.List (intercalate)

unparseLiteral :: O.Literal -> [String]
unparseLiteral lit = case lit of
  O.Int i -> [show i]
  O.Real x -> [show x]
  O.Bool b -> [show b]
  O.EmptyList -> ["emptylist"]
  O.Nil -> ["nil"]
  O.String s -> ["string", (show $ length s)] ++ (map (show . ord) s)

concatSp :: [[Char]] -> [Char]
concatSp = intercalate " "

unparseInstr :: O.Instr -> [Char]
unparseInstr i = case i of
  O.Regs op regs -> concatSp $ op : (map show regs)
  O.RegsLit op regs lit ->
    concatSp $ op : (map show regs) ++ unparseLiteral lit
  O.Goto offset -> concatSp $ ["goto", show offset]
  O.RegInt op r1 r2 offset -> concatSp $ [op, show r1, show r2, show offset]
  O.LoadFunc _ _ _ -> error "LoadFunc reached instr"

list :: [Char] -> [O.Instr] -> [[Char]] -> [[Char]]
list prefix body tail =
  concatSp [prefix, show $ length body] : foldr add tail body

add :: O.Instr -> [[Char]] -> [[Char]]
add i tail = case (i, tail) of
  (O.LoadFunc reg k body, tail) ->
    list (concatSp [".load", show reg, "function", show k]) body tail
  (i, tail) -> unparseInstr i : tail

unparseModule code = list ".load module" code []