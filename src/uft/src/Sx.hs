module Sx where

data Sx = Int Int
        | Bool Bool
        | Sym String
        | Real Double
        | List [Sx]
        deriving Show

whatis x = case x of
  Int _ -> "an integer"
  Bool _ -> "a Boolean"
  Sym _ -> "a symbol"
  Real _ -> "a real number"
  List _ -> "a list"