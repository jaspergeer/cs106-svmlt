-- Embeds KNormal-form Scheme into VScheme. This cannot fail.
module KnEmbed where

import qualified KNF as K
import qualified VScheme as S
import qualified Primitives as P
import qualified ObjectCode as O

-- The value function is actually a projection, not an embedding: 
-- the VM code supports floating-point values, string values and nil, 
-- neither of which can be written as vScheme literals. Nonetheless, 
-- we’re going to treat it as if it were an embedding—we’re going to cheat.

-- Embed a real value as its nearest integer, using function Real.round. (NO)

-- Embed a string value as a symbol (lame, but the best we can do).

-- Embed nil as the Boolean false.

value :: K.Literal -> S.Value
-- two cheating cases
value (O.String s) = S.Sym s
value O.Nil = S.Bool False
value (O.Int i) = S.Int i
value (O.Real r) = S.Real r
value (O.Bool b) = S.Bool b
value O.EmptyList = S.EmptyList

-- The definition embedding uses only one definition form, VScheme.EXP. 
-- Internally, the main embedding should be
-- val exp : VScheme.name KNormalForm.exp -> VScheme.exp

-- exp :: K.Exp S.Name -> S.Exp
-- exp (K.Literal l) = S.Literal (value l)
-- exp (K.Name x) = S.Var x
-- exp (K.IfX e1 e2 e3) = S.IfX (exp e1) (exp e2) (exp e3)
-- exp (K.Let n [(x', e')] e) = let' x' e' (exp e)
-- exp (K.Seq e1 e2) = S.Begin [exp e1, exp e2]
-- exp (K.Assign x e) = S.Set x (exp e)
-- exp (K.While e1 e2) = S.WhileX (exp e1) (exp e2)
-- exp (K.FunCode xs e) = S.Lambda xs (KnEmbed.exp e)
-- -- not sure for VMOP, VMOPGLO
-- exp (K.VMOP op args) = S.Apply (S.Var (P.name op)) (map S.Var args)
-- exp (K.VMOPGLO op args) = S.Apply (S.Var (P.name op)) (map S.Var args)

let' x e' = S.LetX S.Let [(x, e')]

def :: K.Exp S.Name -> S.Def
def e = S.Exp (exp e)
    where   exp e = case e of
                K.Literal l -> S.Literal (value l)
                K.Name x -> S.Var x
                K.If x e2 e3 -> S.IfX (S.Var x) (exp e2) (exp e3)
                K.Let x e' e -> let' x (exp e') (exp e)
                K.Seq (K.VMOPGLO op [x] (O.String v)) e2
                  | P.name op == "setglobal" -> S.Set v (S.Var x)
                K.Seq e1 e2 -> S.Begin [exp e1, exp e2]
                K.Assign x e -> S.Set x (exp e)
                K.While x e1 e2 -> S.WhileX (let' x (exp e1) (S.Var x)) (exp e2)
                K.FunCode xs e -> S.Lambda xs (exp e)
            -- not sure for VMOP, VMOPGLO
                K.VMOP op args -> S.Apply (S.Var (P.name op)) (map S.Var args)
            -- getglobal case
                K.VMOPGLO op xs v -> case (P.name op, xs, v) of
                    ("getglobal", [], O.String v) -> S.Var v
                    ("getglobal", [r], O.String v) -> S.Var v
                    ("setglobal", [x], O.String v) -> S.Set v (S.Var x)

                    -- match check and expect
                    -- may cause undefined behavior
                    _ -> S.Apply (S.Var (P.name op)) (map S.Var xs ++ [S.Literal (value v)])
                K.FunCall f args -> S.Apply (S.Var f) (map S.Var args)
                K.Captured i -> S.Apply (S.Var "CAPTURED-IN") [S.Literal $ S.Int i, S.Var "$closure"]
                K.ClosureX (K.Closure formals body captured) ->
                    let mkclosure = S.Apply (S.Var "mkclosure")
                        cons x y = S.Apply (S.Var "cons") [x, y]
                    in mkclosure [S.Lambda ("$closure" : formals) (exp body),
                                  foldr (\e l -> cons (exp e) l) (S.Literal S.EmptyList) (map K.Name captured)] 
                                  -- change captured because the KNF representation is [a] rather than [Exp a], 
                                  -- use the former to be consistent with modele 10 handout
                K.LetRec bindings body -> 
                    let
                        mkclosure = S.Apply (S.Var "mkclosure")
                        cons x y = S.Apply (S.Var "cons") [x, y]
                        bindings' = map (\(f, closure) -> (f, exp (K.ClosureX closure))) bindings
                        body' = exp body
                    in S.LetX S.LetRec bindings' body'
                K.Block xs -> S.Apply (S.Var "block") (map S.Var xs)
                K.SwitchVCon x choices e -> 
                    let lastQa = [(S.Literal (S.Bool True), exp e)]
                        isCon (vcon, arity) = S.Apply (S.Var "marches-vcon-arity?") [S.Var x, S.Literal (S.Sym vcon), S.Literal (S.Int arity)]
                        qa (c, e) = (isCon c, exp e)
                     in S.Cond (map qa choices ++ lastQa)

-- What will happen if the offset of the VMOPL is not a string in getglobal case:
-- Ans: impossible, should have some error message for debugging,


-- Embed the SWITCH_VCON and BLOCK forms. In file knembed.sml, update your K-normal form embedding to handle the new syntactic forms. Feel free to adapt my code:

-- | embed (K.BLOCK xs) = S.APPLY (S.VAR "block", map S.VAR xs)
-- | embed (K.SWITCH_VCON (x, choices, e)) =
--     let val lastQa = [(S.LITERAL (S.BOOLV true), embed e)]
--         fun isCon (vcon, arity) =
--               S.APPLY (S.VAR "matches-vcon-arity?",
--                        [S.VAR x, S.LITERAL (S.SYM vcon), (S.LITERAL o S.INT) arity])
--         fun qa (c, e) = (isCon c, embed e)
--     in  S.COND (map qa choices @ lastQa)
--     end
