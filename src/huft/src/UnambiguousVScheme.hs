module UnambiguousVScheme where

-- in vscheme.sml in norman's uft

import qualified Primitives as P
import qualified VScheme as S

import qualified Constructed
import qualified Case

data LetKind = Let | LetRec deriving Show

type Name = String

data Lambda = Lambda [Name] Exp deriving Show

data Exp = Literal Value
         | Local Name
         | Global Name
         | SetLocal Name Exp
         | SetGlobal Name Exp
         | IfX Exp Exp Exp
         | WhileX Exp Exp
         | Begin [Exp]
         | FunCall Exp [Exp]
         | PrimCall P.Primitive [Exp]
         | LetX LetKind [(Name, Exp)] Exp
         | LambdaX Lambda
         | Case    (Case.T Exp)
         | Constructed (Constructed.T Exp)
         deriving Show

data Value = Sym Name
           | Int Int
           | Real Double
           | Bool Bool
           | EmptyList

instance Show Value where
  show v = case v of
    Sym n -> n
    Int i -> show i
    Real x -> show x
    Bool b -> show b
    EmptyList -> "'()"

data Def = Val Name Exp
         | Define Name [Name] Exp
         | Exp Exp
         | Use Name
         | CheckExpect String Exp String Exp
         | CheckAssert String Exp
         deriving Show

whatIs :: Exp -> String
whatIs (Literal v) = "literal " ++ show v
whatIs (Local n) = "local " ++ n
whatIs (Global n) = "global " ++ n
whatIs (SetLocal n e) = "set local " ++ n ++ " to " ++ whatIs e
whatIs (SetGlobal n e) = "set global " ++ n ++ " to " ++ whatIs e
whatIs (IfX e1 e2 e3) = "if"
whatIs (WhileX e1 e2) = "while"
whatIs (Begin es) = "begin"
whatIs (FunCall e es) = "fun call"
whatIs (PrimCall p es) = "prim call " ++ P.name p
whatIs (LetX Let bs e) = "let"
whatIs (LetX LetRec bs e) = "letrec"
whatIs (LambdaX (Lambda xs e)) = "lambda"
