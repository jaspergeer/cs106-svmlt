-- Embeds KNormal-form Scheme into VScheme. This cannot fail.
module KnEmbed where

import qualified KNF as K
import qualified VScheme as S
import qualified Primitives as P
import qualified ObjectCode as O

let' x e' e = S.LetX S.Let [(x, e')] e

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
value (O.EmptyList) = S.EmptyList

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

def :: K.Exp S.Name -> S.Def
def e = S.Exp (exp e)
    where   exp (K.Literal l) = S.Literal (value l)
            exp (K.Name x) = S.Var x
            exp (K.If x e2 e3) = S.IfX (S.Var x) (exp e2) (exp e3)
            exp (K.Let x e' e) = let' x (exp e') (exp e)
            exp (K.Seq e1 e2) = S.Begin [exp e1, exp e2]
            exp (K.Assign x e) = S.Set x (exp e)
            exp (K.While x e1 e2) = S.WhileX (let' x (exp e1) (S.Var x)) (exp e2)
            exp (K.FunCode xs e) = S.Lambda xs (exp e)
            -- not sure for VMOP, VMOPGLO
            exp (K.VMOP op args) = S.Apply (S.Var (P.name op)) (map S.Var args)
            -- getglobal case
            exp (K.VMOPGLO op [] v) = case ((P.name op), v) of
                ("getglobal", (O.String v)) -> 
                        S.Apply (S.Var (P.name op)) [S.Literal (S.Sym v)]
                _ -> S.Apply (S.Var (P.name op)) (S.Literal (value v):[])
            exp (K.VMOPGLO op (x:xs) v) = case (P.name op) of
                "setglobal" -> S.Set x (S.Literal (value v))
                _ -> S.Apply (S.Var (P.name op)) (map S.Var (x:xs))