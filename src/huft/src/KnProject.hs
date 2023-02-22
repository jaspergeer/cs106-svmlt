module KnProject where
import Prelude hiding ( exp )
-- Project disambiguated VScheme into KNormal representation. 
-- Note that this can fail if the disambiguated VScheme is not already 
-- written in KNormal-form.

import qualified KNF as K
import qualified Primitives as P
import qualified UnambiguousVScheme as X
import qualified Error as E
import qualified ObjectCode as O

asName :: X.Exp -> E.Error X.Name
asName (X.Local x) = return x
asName e           = E.Error $ Left ("expected a local variable but instead got" ++ X.whatIs e)

-- val value : UnambiguousVScheme.value -> KNormalForm.literal
-- val def   : UnambiguousVScheme.def -> string KNormalForm.exp Error.error

value :: X.Value -> K.Literal
value (X.Sym s) = O.String s
value (X.Int i) = O.Int i
value (X.Real r) = O.Real r
value (X.Bool b) = O.Bool b
value X.EmptyList = O.EmptyList

-- What is the string
exp :: X.Exp -> E.Error (K.Exp String)
exp e = case e of
  X.Literal v -> return $ K.Literal (value v)
  X.Local x -> return $ K.Name x
  X.Global x -> return $ K.getglobal x
  X.SetLocal x e -> K.Assign x <$> exp e
  X.SetGlobal x x' -> K.setglobal x <$>  asName x'
  X.IfX e1 e2 e3 -> K.If <$> asName e1 <*> exp e2 <*> exp e3
  X.WhileX (X.LetX X.Let [(x , e1)] (X.Local x')) e2 -> if x /= x' then E.Error $ Left "names don't match"
    else K.While x <$> exp e1 <*> exp e2
  X.Begin [e1, e2] -> K.Seq <$> exp e1 <*> exp e2
  X.LetX X.Let [(x, e)] e' -> K.Let x <$> exp e <*> exp e'
-- Any lambda form except for the special case of a global function definition, which should be handled by the def function
  X.PrimCall p es -> case (P.name p, es) of
    ("check", [e, X.Literal v]) -> K.VMOPGLO p <$> mapM asName [e] <*> return (value v)
    ("expect", [e, X.Literal v]) -> K.VMOPGLO p <$> mapM asName [e] <*> return (value v)
    (_ , es)       -> K.VMOP p <$> mapM asName es
  X.FunCall e es -> K.FunCall <$> asName e <*> mapM asName es
  _ -> E.Error $ Left "Cannot project to KNF"

def :: X.Def -> E.Error (K.Exp String)
def d = case d of
  X.Exp (X.LetX X.Let [(t, X.Lambda xs e)] (X.SetGlobal t' f)) -> if t /= t' 
      then E.Error $ Left "names don't match"
      else K.Let t <$> (K.FunCode xs <$> exp e) <*> (K.setglobal t <$> asName f)
  X.Exp e -> exp e
  X.Define f xs e -> K.Let t  <$> (K.FunCode xs <$> exp e) <*> return (K.setglobal t f)
  _ -> E.Error $ Left "Cannot project to KNF"
  where t = "$nr"