module FOScheme where
import qualified ObjectCode as O
import qualified Primitives as P

import qualified Case
import qualified Constructed

type Name = String

-- why uft has 2 datatype keywords
type Literal = O.Literal

data Exp = Literal Literal
         | Local  Name
         | Global Name
         | SetLocal Name Exp
         | SetGlobal Name Exp
         | IfX Exp Exp Exp
         | WhileX Exp Exp
         | Begin [Exp] 
         | FunCall Exp [Exp]
         | PrimCall P.Primitive [Exp]
         | Let [(Name, Exp)] Exp
        -- module 12
         | Constructed (Constructed.T Exp)
         | Case (Case.T Exp)
         deriving Show

data Def = Val Name Exp
         | Define Name [Name] Exp
         | Exp Exp
         | CheckExpect String Exp String Exp
         | CheckAssert String Exp
         deriving Show