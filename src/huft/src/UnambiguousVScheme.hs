module UnambiguousVScheme where

import qualified Primitives as P
import qualified VScheme as S

data LetKind = Let | LetRec deriving Show

type Name = String

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
         | Lambda [Name] Exp
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
         | CheckExpect String Exp String Exp
         | CheckAssert String Exp
         deriving Show