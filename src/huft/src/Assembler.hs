module Assembler where

import qualified Asm as A
import qualified ObjectCode as O
import Error
import qualified Env as E

foldrInstrStream :: (Int -> A.Instr -> a -> a) -> a -> [A.Instr] -> a
foldrInstrStream f e instrs = let
  fris _ accum [] ct = accum
  fris f accum (i:is) ct = f ct i (fris f accum is
    (ct + case i of
      A.IfGotoLabel {} -> 2
      A.DefLabel _ -> 0
      _ -> 1))
  in fris f e instrs 0 

lift3 :: (a -> b -> c -> Error c) -> a -> b -> Error c -> Error c
lift3 f a b ce = ce >>= f a b

labelEnv :: [A.Instr] -> Error (E.Env Int)
labelEnv = foldrInstrStream (lift3 f) (Error $ Right E.empty) where
  f pos (A.DefLabel n) env = if env `E.binds` n
    then Error $ Left ("label '" ++ n ++ "' defined in more than one place")
    else Error $ Right (E.bind n pos env)
  f _ _ env = Error $ Right env

-- The mutual recursion works like this:

-- Function labelElim calls translate when it needs to eliminate labels from the body of a LOADFUNC form.

-- Label elimination takes two passes:
-- The first pass computes and records the position of every label.
-- The second pass replaces assembly-language GOTO_LABEL, which branches 
-- to a label, with an object-code GOTO, which branches relative 
-- to the position of the GOTO. And it discards the labels.

labelElim :: [A.Instr] -> E.Env Int -> Error [O.Instr]
labelElim instrs env = let
  f pos i is = case i of
    -- loadfunc
    (A.LoadFunc reg arity body) -> (:) <$>
      (O.LoadFunc reg arity <$> translate body) <*> is
    -- goto
    (A.GotoLabel n) -> 
      E.find n env >>= \x -> (O.Goto (x - pos - 1) :) <$> is
    (A.IfGotoLabel r1 n) -> 
      E.find n env >>= \x -> ([O.Regs "cskip" [r1], O.Goto (x - pos - 1)] ++) <$> is
    -- base cases
    (A.ObjectCode o) -> (o :) <$> is
    (A.DefLabel _ ) -> is
  in foldrInstrStream f (Error $ Right []) instrs

translate :: [A.Instr] -> Error [O.Instr]
translate instrs = labelEnv instrs >>= labelElim instrs