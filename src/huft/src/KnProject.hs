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

asName :: X.Exp -> E.Error K.Name
asName (X.Local x) = return x
asName e           = E.Error $ Left ("expected a local variable but instead got" ++ X.whatIs e)

-- val value : UnambiguousVScheme.value -> KNormalForm.literal
-- val def   : UnambiguousVScheme.def -> string KNormalForm.exp Error.error

value :: X.Value -> K.Literal
value (X.Sym s) = O.String s
value (X.Int i) = O.Int i
value (X.Real r) = O.Real r
value (X.BoolV b) = O.Bool b
value X.EmptyList = O.EmptyList

exp :: X.Exp -> E.Error (K.Exp K.Name)
exp (X.Literal v) = return $ K.Literal (value v)
exp (X.Local x) = return $ K.Name x
exp (X.Global x) = return $ K.Name x
exp (X.SetLocal x e) = do
  e' <- exp e
  return $ K.Assign x e'

def :: X.Def -> E.Error (K.Exp K.Name)
def = undefined