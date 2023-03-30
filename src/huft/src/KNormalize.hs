-- KNormalizer from FO Scheme to KNF
-- this is where register allocation a happens

module KNormalize where

import qualified KNF as K
import qualified FOScheme as F
import qualified Env as E
import qualified Primitives as P

type Reg = Int

regname :: Reg -> String
regname r = "$r" ++ show r

newtype RegSet = RS Int

-- K normalization

type Exp = K.Exp Reg
type Policy = RegSet -> Exp -> (RegSet -> Exp) -> Exp
-- puts the expression in an register, continues

type Normalizer a = RegSet -> a -> Exp

-- what is this A for?
nbRegsWith normalize bind a xs k = undefined

exp :: E.Env Reg -> RegSet -> F.Exp -> K.Exp Reg
exp rho a e = undefined

def :: F.Def -> K.Exp Reg
def e = undefined