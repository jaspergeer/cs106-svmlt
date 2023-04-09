-- correspond to closure-convert.sml

module ClConvert where

import qualified UnambiguousVScheme as X
import qualified ClScheme as C
import qualified Data.Set as S
import qualified ObjectCode as O
import qualified Primitives as P

-- nr's set signature

--   type 'a set
--   val empty : 'a set
--   val member : ''a * ''a set -> bool
--   val insert : ''a * ''a set -> ''a set
--   val diff : ''a set * ''a set -> ''a set
--   val elems : 'a set -> 'a list
--   val ofList : ''a list -> ''a set

--   val union' : ''a set list -> ''a set  (* union of a list of sets *)

-- need to import O, cannot inherit the value constructor like the datatype
-- keyword in sml (* datatype literal = datatype ObjectCode.literal *)

literal lit = case lit of
    (X.Sym x) -> C.Literal (O.String x)
    (X.Int x) -> C.Literal (O.Int x)
    (X.Real x) -> C.Literal (O.Real x)
    (X.Bool x) -> C.Literal (O.Bool x)
    X.EmptyList -> C.Literal O.EmptyList


-- (* returns `SOME i`, where i is the position of `x` in `xs`,
--     or if `x` does not appear in `xs`, returns `NONE` *)
indexOf :: X.Name -> [X.Name] -> Maybe Int
indexOf x xs =
    let find k [] = Nothing
        find k (y : ys) = if x == y then Just k else find (k + 1) ys
    in find 0 xs

-- (* Given an expression `e` in Unambiguous vScheme, plus a list
--     of the free variables of that expression, return the closure-
--     converted version of the expression in Closed Scheme *)
closedExp :: [X.Name] -> X.Exp -> C.Exp
closedExp captured e = 
    let --  (* I recommend internal function exp : X.exp -> C.exp *)
        closure :: X.Lambda -> C.Closure
        closure (xs, body) =
            let freevars = S.toList (free body) 
            -- maybe S.toList $ S.difference (free body) (S.fromList xs) ?
                cons x y = C.PrimCall P.cons [x, y]
            in C.Closure xs (closedExp freevars e)
                            (map (\x -> case indexOf x captured of
                                Just i -> C.Captured i
                                Nothing -> C.Local x) freevars)

        exp :: X.Exp -> C.Exp
        exp e = case e of
            (X.Literal lit) -> literal lit
            (X.Local x) -> case indexOf x captured of
                (Just i) -> C.Captured i
                Nothing -> C.Local x
            (X.Global x) -> C.Global x
            (X.SetLocal x e) -> C.SetLocal x (exp e)
            (X.SetGlobal x e) -> C.SetGlobal x (exp e)
            (X.IfX e1 e2 e3) -> C.IfX (exp e1) (exp e2) (exp e3)
            (X.WhileX e1 e2) -> C.WhileX (exp e1) (exp e2)
            (X.Begin es) -> C.Begin (map exp es)
            (X.FunCall e1 es) -> C.FunCall (exp e1) (map exp es)
            (X.PrimCall p es) -> C.PrimCall p (map exp es)
            (X.LetX X.Let bs e) -> C.Let (map (\(x, e) -> (x, exp e) ) bs) (exp e)
            (X.LetX X.LetRec bs e) -> undefined
            (X.Lambda xs e) -> C.ClosureX (closure (xs, e))
    in exp e

free :: X.Exp -> S.Set X.Name
free e = case e of
    (X.Literal lit) -> S.empty
    (X.Local x) -> S.singleton x
    (X.Global x) -> S.empty  -- in the jargon of closure conversion, a global variable is not a free variable.
    (X.SetLocal x e) -> S.union (S.singleton x) (free e)
    (X.SetGlobal x e) -> S.union (S.singleton x) (free e) -- not sure
    (X.IfX e1 e2 e3) -> S.union (free e1) (S.union (free e2) (free e3))
    (X.WhileX e1 e2) -> S.union (free e1) (free e2)
    (X.Begin es) -> S.unions (map free es)
    (X.FunCall e1 es) -> S.unions (free e1 : map free es)
    (X.PrimCall p es) -> S.unions (map free es)
    (X.LetX X.Let xs e) -> S.union (S.unions (map (free . snd) xs)) 
                                   (S.difference (free e) 
                                                 (S.fromList (map fst xs)))
    (X.LetX X.LetRec xs e) -> S.difference (S.union (S.unions (map (free . snd) xs)) (free e))
                                           (S.fromList (map fst xs))
    (X.Lambda xs e) -> S.difference (free e) (S.fromList xs) 

close :: X.Def -> C.Def
close d = case d of
    (X.Exp e) -> C.Exp (closedExp [] e)
    (X.Val x e) -> C.Val x (closedExp [] e)
    (X.CheckExpect s1 e1 s2 e2) -> C.CheckExpect s1 (closedExp [] e1) s2 (closedExp [] e2)
    (X.CheckAssert s e) -> C.CheckAssert s (closedExp [] e)
    (X.Define n ns e) -> C.Define n (C.FunCode ns (closedExp [] e)) 