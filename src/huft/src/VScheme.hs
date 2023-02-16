module VScheme where

type Name = String

data Exp = Literal Value
         | Var     Name
         | Set     Name Exp
         | IfX     Exp Exp Exp
         | WhileX  Exp Exp
         | Begin   [Exp]
         | Apply   [(Exp, Exp)]
         | LetX    LetKind [(Name, Exp)] Exp
         | Lambda  Lambda

data LetKind = Let | LetRec

data Value = Sym   Name
           | Int   Int
           | REAL  Double
           | BoolV Bool
           | Pair  Value Value
           | EmptyList
type Lambda = ([Name], Exp)

data DefLabel  = Val         Name Exp
               | Define      Name Lambda
               | Exp         Exp
               | CheckExpect Exp Exp
               | CheckAssert Exp