module Pattern where

type VCon = String
type Name = String

data Pat = Apply VCon [Pat]
         | Int Int
         | Var Name
         | Wildcard
         deriving (Eq, Show)

instance Ord Pat where
  Wildcard <= _ = True
  Var x <= Var y = x <= y
  Int x <= Int y = x <= y
  Var _ <= Int _ = True
  Int _ <= Apply {} = True
  Apply x _ <= Apply y _ = x <= y 
  _ <= _ = False