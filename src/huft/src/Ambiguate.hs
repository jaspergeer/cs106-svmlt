module Ambiguate where
import qualified Primitives as P
import qualified VScheme as S
import qualified UnambiguousVScheme as X
import qualified Case
import qualified Constructed

-- re-embedding into ambiguous code

devalue lit = case lit of
  X.Sym x -> S.Sym x
  X.Int n -> S.Int n
  X.Real n -> S.Real n
  X.Bool b -> S.Bool b
  X.EmptyList -> S.EmptyList

maybeValue :: X.Exp -> Maybe S.Value
maybeValue lit = case lit of
  X.Literal v -> Just (devalue v)
  X.PrimCall opr [e1, e2] -> case (P.name opr, maybeValue e1, maybeValue e2) of
    ("cons", Just y, Just ys) -> Just (S.Pair y ys)
    _ -> Nothing
  _ -> Nothing

exp' :: X.Exp -> S.Exp
exp' e = case maybeValue e of
  Just v -> S.Literal v
  Nothing -> exp e
  where
  exp e = case e of
    X.FunCall f args -> S.Apply (exp f) (map exp args)
    X.PrimCall p es -> S.Apply (S.Var (P.name p)) (map exp es)
    X.Local x -> S.Var x
    X.Global x -> S.Var x
    X.SetLocal x e -> S.Set x (exp e)
    X.SetGlobal x e -> S.Set x (exp e)
    X.Literal v -> S.Literal (devalue v)
    X.IfX e1 e2 e3 -> S.IfX (exp e1) (exp e2) (exp e3)
    X.LetX X.Let bs e -> S.LetX S.Let (map binding bs) (exp e)
    X.LetX X.LetRec bs e -> S.LetX S.LetRec (map binding bs) (exp e)
    X.Begin es -> S.Begin (map exp es)
    X.WhileX c body -> S.WhileX (exp c) (exp body)
    X.LambdaX (X.Lambda xs e) -> S.Lambda xs (exp e)
    X.Case c -> S.Case (fmap exp c)
    X.Constructed (Constructed.T vcon es) -> S.Apply (S.VCon vcon) (map exp es)
  binding (x, e) = (x, exp e)

def d = case d of
  X.Exp e -> S.Exp (exp' e)
  X.Val x e -> S.Val x (exp' e)
  X.Define f xs e -> S.Define f xs (exp' e)
  X.CheckExpect _ e _ e' -> S.CheckExpect (exp' e) (exp' e')
  X.CheckAssert _ e -> S.CheckAssert (exp' e)

ambiguate :: X.Def -> S.Def
ambiguate = def
