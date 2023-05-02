-- embedding first-order Scheme to Closed Scheme, This cannot failed
-- used to read closed scheme from a file (piggyback on the FO reader)

module FOClUtil where

import Prelude hiding ( exp )
import qualified ClScheme as C
import qualified FOScheme as F

import qualified Case
import qualified Constructed

exp :: F.Exp -> C.Exp
exp e = 
    let binding (x, e) = (x, exp e)
    in case e of
    (F.Literal v) -> C.Literal v
    (F.Local x) -> C.Local x
    (F.Global x) -> C.Global x
    (F.IfX e1 e2 e3) -> C.IfX (exp e1) (exp e2) (exp e3)
    (F.PrimCall p es) -> C.PrimCall p (map exp es)
    (F.FunCall e es) -> C.FunCall (exp e) (map exp es)
    (F.Let bs e) -> C.Let (map binding bs) (exp e)
    (F.Begin es) -> C.Begin (map exp es)
    (F.SetLocal x e) -> C.SetLocal x (exp e)
    (F.SetGlobal x e) -> C.SetGlobal x (exp e)
    (F.WhileX c body) -> C.WhileX (exp c) (exp body)
    (F.Case c) -> C.Case (fmap exp c)
    (F.Constructed (Constructed.T con es)) -> C.Constructed (Constructed.T con (map exp es))


def :: F.Def -> C.Def
def d = case d of
    (F.Val x e) -> C.Val x (exp e)
    (F.Exp e) -> C.Exp (exp e)
    (F.Define f xs e) -> C.Define f (C.FunCode xs (exp e))
    (F.CheckExpect s e s' e') -> C.CheckExpect s (exp e) s' (exp e')
    (F.CheckAssert s e) -> C.CheckAssert s (exp e)

embed :: F.Def -> C.Def
embed = def
