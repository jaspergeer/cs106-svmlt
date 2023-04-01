module Disambiguate where
import qualified Primitives as P
import qualified VScheme as S
import qualified UnambiguousVScheme as X
import qualified VSchemeUnparse as U

data Referent = Local | Primitive P.Primitive | OtherGlobal

type Name = String

-- The rules for disambiguating a function application are as follows:

-- If the function is a local variable, the application becomes a FUNCALL form,
-- with a LOCAL form of exp in the function position.

-- If the function is a variable that is nonlocal and that names a primitive, 
-- the application becomes a PRIMCALL form, to the named primitive.

-- If the function is any other form, including a global variable that is not a 
-- primitive, application becomes a FUNCALL form, with the original form of 
-- exp in the function position.

-- The rules for disambiguating VAR and SET forms are simpler: 
-- determine the name’s referent and choose the appropriate LOCAL 
-- or GLOBAL form in the target language. 
-- There’s one exception: if the name of a primitive function 
--   is used outside of a function position, it must be eta-expanded. 
--   For example, in

type Environment = [Name]

referent :: Name -> Environment -> Referent
referent x locals =
  if x `elem` locals then Local
  else maybe OtherGlobal Primitive (P.find x)

-- used when a primitive occurs in a value position
etaExpand :: P.Primitive -> X.Exp
etaExpand p = X.Lambda args (X.PrimCall p (map X.Local args))
      where args = map (\i -> "x" ++ show i) [1..P.arity p]-- translation of literals

value :: S.Value -> X.Exp
value v = case v of
  S.Pair v1 v2 -> X.PrimCall P.cons [value v1, value v2]
  S.Int n -> X.Literal (X.Int n)
  S.Real n -> X.Literal (X.Real n)
  S.Bool b -> X.Literal (X.Bool b)
  S.EmptyList -> X.Literal X.EmptyList
  S.Sym x -> X.Literal (X.Sym x)

exp' :: S.Exp -> Environment -> X.Exp
exp' e locals =
  let exp e = case e of
        S.Literal v -> value v
        S.Var x -> case referent x locals of
          Local -> X.Local x
          Primitive p -> etaExpand p
          OtherGlobal -> X.Global x
        S.Set x e -> case referent x locals of
          Local -> X.SetLocal x (exp e)
          Primitive _ -> X.SetGlobal "it" (exp e)  -- undefined behavior, too lazy to throw error
          OtherGlobal -> X.SetGlobal x (exp e)
        S.IfX e1 e2 e3 -> X.IfX (exp e1) (exp e2) (exp e3)
        S.WhileX e1 e2 -> X.WhileX (exp e1) (exp e2)
        S.Begin es -> X.Begin (map exp es)
        S.Apply e es -> case e of
          S.Var x -> case referent x locals of
            Primitive p -> X.PrimCall p (map exp es)
            _ -> X.FunCall (exp e) (map exp es)
          _ -> X.FunCall (exp e) (map exp es)
        S.LetX S.Let bindings e ->
          let bs = map (\(x, e) -> (x, exp e)) bindings
              e' = exp' e (map fst bindings ++ locals)
          in X.LetX X.Let bs e'
        S.LetX S.LetRec bindings e ->
          let locals = map fst bindings ++ locals
              bs = map (\(x, e) -> (x, exp' e locals)) bindings
              e' = exp' e locals
          in X.LetX X.LetRec bs e'
        S.Lambda xs e -> X.Lambda xs (exp' e (xs ++ locals))
  in exp e

def :: S.Def -> X.Def
def d = case d of
  S.Define f xs e -> X.Define f xs (exp' e xs)
  S.Val f (S.Lambda xs e) -> X.Define f xs (exp' e xs)
  S.Val x e -> X.Val x (exp' e [])
  S.Exp e -> X.Exp (exp' e [])
  S.CheckExpect e e' -> X.CheckExpect (U.ppexp e) (exp' e []) (U.ppexp e') (exp' e' [])
  S.CheckAssert e -> X.CheckAssert (U.ppexp e) (exp' e [])

disambiguate :: S.Def -> X.Def
disambiguate = def