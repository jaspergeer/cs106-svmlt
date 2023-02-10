module Assembler where

import qualified Asm as A
import qualified ObjectCode as O
import Error ( Error )
import qualified Env as E

translate :: [A.Instr] -> [O.Instr]
translate = error "implement me"

-- val fold : (int * AssemblyCode.instr * 'a -> 'a) -> 'a -> AssemblyCode.instr list -> 'a

foldInstStream :: (Int -> A.Instr -> a -> a) -> a -> [A.Instr] -> a
foldInstStream f init instrs = fis f init instrs 0 where
  fis _ accum [] ct = accum
  fis f accum (i:is) ct = fis f (f ct i accum) is
    (ct + case i of
      A.IfGotoLabel {} -> 2
      A.DefLabel _ -> 0
      _ -> 1)

lift3 :: (a -> b -> c -> Error c) -> a -> b -> Error c -> Error c
lift3 f a b (Left c) = Left c
lift3 f a b (Right c) = f a b c

labelEnv :: [A.Instr] -> Error (E.Env Int)
labelEnv = foldInstStream (lift3 f) (Right E.empty) where
  f pos (A.DefLabel n) env = if env `E.binds` n
    then Left ("label '" ++ n ++ "' defined in more than one place")
    else Right (E.bind n pos env)
  f _ _ env = Right env

