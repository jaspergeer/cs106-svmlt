module KnProject where

-- Project disambiguated VScheme into KNormal representation. 
-- Note that this can fail if the disambiguated VScheme is not already 
-- written in KNormal-form.

import qualified KNF as K
import qualified Primitives as P
import qualified UnambiguousVScheme as X
import qualified Error as E

asName :: X.Exp -> E.Error K.Name
asName (X.Local x) = return x
asName _           = E.Error $ Left "asName: not a name" -- TODO

-- val value : UnambiguousVScheme.value -> KNormalForm.literal
-- val def   : UnambiguousVScheme.def -> string KNormalForm.exp Error.error
