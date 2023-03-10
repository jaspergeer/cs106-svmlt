module ObjectUnparser where
import qualified ObjectCode as O
import Data.Char (ord, toLower)

unparseLiteral :: O.Literal -> [String]
unparseLiteral lit = case lit of
  O.Int i -> [show i]
  O.Real x -> [show x]
  O.Bool b -> [map toLower (show b)]
  O.EmptyList -> ["emptylist"]
  O.Nil -> ["nil"]
  O.String s -> ["string", show (length s)] ++ map (show . ord) s

unparseInstr :: O.Instr -> [Char]
unparseInstr i = case i of
  O.Regs op regs -> unwords (op : map show regs)
  O.RegLit op r1 lit ->
    unwords (op : show r1 : unparseLiteral lit)
  O.RegGlo op r1 n ->
    unwords (op : show r1 : unparseLiteral (O.String n))
  O.Goto offset -> unwords ["jump", show offset]
  O.RegsInt op regs offset -> unwords ((op : map show regs) ++ [show offset])
  O.LoadFunc {} -> error "IMPOSSIBLE: LoadFunc reached instr"

unparseFunc :: [Char] -> [O.Instr] -> [[Char]] -> [[Char]]
unparseFunc prefix body tail =
  unwords [prefix, show (length body)] : foldr add tail body

add :: O.Instr -> [[Char]] -> [[Char]]
add i tail = case (i, tail) of
  (O.LoadFunc reg k body, tail) ->
    unparseFunc (unwords [".load", show reg, "function", show k]) body tail
  (i, tail) -> unparseInstr i : tail

unparseModule code = unparseFunc ".load module" code []