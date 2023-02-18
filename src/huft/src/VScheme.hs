module VScheme where

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

data LetKind = Let | LetRec

data Value = Sym   Name
           | Int   Int
           | REAL  Double
           | BoolV Bool
           | Pair  Value Value
           | EmptyList

data DefLabel  = Val         Name Exp
               | Define      Name [Name] Exp
               | Exp         Exp
               | CheckExpect Exp Exp
               | CheckAssert Exp