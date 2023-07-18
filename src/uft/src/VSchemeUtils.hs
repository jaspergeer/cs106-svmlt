module VSchemeUtils where

import qualified VScheme as S

car e = S.Apply (S.Var "car")  [e]
cdr e = S.Apply (S.Var "cdr") [e]
cons x xs = S.Apply (S.Var "cons") [x, xs]
setcar e v = S.Apply (S.Var "set-car!") [e, v]
mkclosure code freevars = S.Apply (S.Var "mkclosure")
 [code, freevars]

nth :: Int -> S.Exp -> S.Exp
nth 0 e = car e
nth k e = nth (k-1) (cdr e)

list :: [S.Exp] -> S.Exp
list = foldr cons (S.Literal S.EmptyList)

setnth :: S.Exp -> Int -> S.Exp -> S.Exp
setnth e 0 v = S.Apply (S.Var "set-car!") [e,v]
setnth e k v = setnth (cdr e) (k - 1) v