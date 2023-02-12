module Assembler where

import qualified Asm as A
import qualified ObjectCode as O
import Error ( Error )
import qualified Env as E

-- fold, lift, labelEnv,  

-- foldlInstStream :: (Int -> A.Instr -> a -> a) -> a -> [A.Instr] -> a
-- foldlInstStream f init instrs =let
--   fis _ accum [] ct = accum
--   fis f accum (i:is) ct = fis f (f ct i accum) is
--     (ct + case i of
--       A.IfGotoLabel {} -> 2
--       A.DefLabel _ -> 0
--       _ -> 1)
--   in fis f init instrs 0


-- In module 4 lab, define `fold`, `lift`, `labelEnv`, `labelElim`, and `translate`


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
labelEnv = foldrInstrStream (lift3 f) (Right E.empty) where
  f pos (A.DefLabel n) env = if env `E.binds` n
    then Left ("label '" ++ n ++ "' defined in more than one place")
    else Right (E.bind n pos env)
  f _ _ env = Right env

-- The mutual recursion works like this:

-- Function labelElim calls translate when it needs to eliminate labels from the body of a LOADFUNC form.

labelElim :: [A.Instr] -> E.Env Int -> Error [O.Instr]
-- use applicative functors
labelElim ((A.LoadFunc reg arity body):is) env = (:) <$> (O.LoadFunc reg arity <$> translate body) <*> labelElim is env
labelElim ((A.DefLabel _ ):is) env = labelElim is env
-- labelElim ((A.GotoLabel n):is) env = (:) <$> O.Goto (E.find n env) <*> labelElim is env
labelElim ((A.GotoLabel n):is) env = (O.RegsInt "jump" [] (E.find n env) :) <$> labelElim is env
-- labelElim ((A.IfGotoLabel r1 n):is) env = ([O.Regs "cskip" [r1], O.RegsInt "jump" [] (E.find n env)] ++) <$> labelElim is env
labelElim ((A.ObjectCode o):is) env = (o :) <$> labelElim is env
labelElim [] env = Right []

translate :: [A.Instr] -> Error [O.Instr]
translate instrs = labelEnv instrs >>= labelElim instrs