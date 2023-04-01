-- KNormalizer from FO Scheme to KNF
-- this is where register allocation a happens

module KNormalize where

import Prelude hiding ( exp )

import qualified KNF as K
import qualified FOScheme as F
import qualified Env as E
import qualified Primitives as P
import qualified ObjectCode as O

type Reg = Int

regname :: Reg -> String
regname r = "$r" ++ show r

newtype RegSet = RS Int

smallest :: RegSet -> Reg
smallest (RS i) = i

(\\) :: RegSet -> Reg -> RegSet
(RS i) \\ x = RS $ max i x

-- K normalization

type Exp = K.Exp Reg
type Policy = RegSet -> Exp -> (RegSet -> Exp) -> Exp
-- puts the expression in an register, continues

type Normalizer a = RegSet -> a -> Exp

-- what is this A for?
nbRegsWith normalize bind a xs k = undefined

helper = undefined

-- use the continuation to decrease a
exp :: E.Env Reg -> RegSet -> F.Exp -> Exp
exp rho a e = case e of
  (F.PrimCall p [e]) -> bindAnyReg a (exp rho a e) (\t -> K.VMOP p [t])
  -- (F.PrimCall p [e1, e2]) -> bindAnyReg a (exp rho a e1) (\t1 -> 
  --                            bindAnyReg (a \\ t1) (exp rho (a \\ t1) e2) (\t2 -> 
  --                            K.VMOP p [t1, t2]))
  (F.Literal x) -> K.Literal x
  _ -> undefined

primcall :: P.Primitive -> [F.Exp] -> Exp
primcall p es = exp E.empty (RS 0) (F.PrimCall p es)

def :: F.Def -> Exp
def e = case e of
    (F.Exp e) -> exp E.empty (RS 0) e
    (F.CheckExpect s1 e1 s2 e2) -> K.Seq (primcall P.check [e1, F.Literal $ O.String s1])
                                         (primcall P.check [e2, F.Literal $ O.String s2])
    (F.CheckAssert s e) -> primcall P.checkAssert [e, F.Literal $ O.String s]

bindAnyReg :: RegSet -> Exp -> (Reg -> Exp) -> Exp
bindAnyReg a e k = case e of
  (K.Name n) -> k n
  _ ->
    let t = smallest a
    in K.Let t e (k t)

