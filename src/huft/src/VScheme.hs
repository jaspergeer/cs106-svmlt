-- seems like it's not neaded for now

module VScheme where

type Name = String

data Exp = LITERAL   Value
         | VAR       Name
         | SET       Name Exp
         | IFX       Exp Exp Exp
         | WHILEX    Exp Exp
         | BEGIN    [Exp]
         | APPLY     Exp [Exp]
         | LETX      Let_Kind [([Name], Exp)] Exp
         | LAMBDA    Lambda

type Lambda = ([Name], Exp)

data Let_Kind = LET | LETREC

data Value = SYM Name
           | INT Int
           | REAL Double
           | BOOLV Bool
           | PAIR Value Value
           | EMPTYLIST

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