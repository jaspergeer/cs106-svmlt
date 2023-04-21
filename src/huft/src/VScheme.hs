module VScheme where

import qualified Pattern as P
import qualified Case

type Name = String

data Exp = Literal Value
         | Var     Name
         | Set     Name Exp
         | IfX     Exp Exp Exp
         | WhileX  Exp Exp
         | Begin   [Exp]
         | Apply   Exp [Exp]
         | LetX    LetKind [(Name, Exp)] Exp
         | Lambda  [Name] Exp
         | VCon    P.VCon
         | Case    (Case.T Exp)
         deriving Show

data LetKind = Let | LetRec deriving Show

data Value = Sym   Name
           | Int   Int
           | Real  Double
           | Bool Bool
           | Pair  Value Value
           | EmptyList
           deriving Show

data Def = Val         Name Exp
         | Define      Name [Name] Exp
         | Exp         Exp
         | Use         Name
         | CheckExpect Exp Exp
         | CheckAssert Exp
         deriving Show