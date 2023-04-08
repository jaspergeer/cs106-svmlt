module CSUtil where

import Prelude hiding ( exp )
import qualified ClScheme as C
import qualified Primitives as P
import qualified VScheme as S
import qualified VSchemeUtils as SU
import qualified KnEmbed

-- Embedding
exp :: C.Exp -> S.Exp
exp e = 
  let binding (x, e) = (x, exp e)
  in case e of
  C.Captured i -> error "aaaaa"
  C.ClosureX (C.Closure formals body captured) -> undefined
  C.LetRec bs e -> error "a"
  C.FunCall f es -> S.Apply (exp f) (map exp es)
  C.PrimCall p es -> S.Apply (S.Var (P.name p)) (map exp es)
  C.Literal v -> S.Literal (KnEmbed.value v)
  C.Local x -> S.Var x
  C.Global x -> S.Var x
  C.IfX e1 e2 e3 -> S.IfX (exp e1) (exp e2) (exp e3)
  C.Let bs e -> S.LetX S.Let (map binding bs) (exp e)
  C.Begin es -> S.Begin (map exp es)
  C.SetLocal x e -> S.Set x (exp e)
  C.SetGlobal x e -> S.Set x (exp e)
  C.WhileX c body -> S.WhileX (exp c) (exp body)

helper = undefined

def :: C.Def -> S.Def
def e = case e of
  C.Exp e -> S.Exp (exp e)
  C.Val x e -> S.Val x (exp e)
  C.Define f (C.FunCode ns e) -> S.Val f (exp (C.ClosureX (C.Closure ns e [])))

embed :: C.Def -> S.Def
embed = def