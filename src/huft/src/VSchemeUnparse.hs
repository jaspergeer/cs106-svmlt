module VSchemeUnparse where
import Prelude hiding ( exp )
import qualified VScheme as S
import qualified Prettyprinter as P
-- sadly HLS cannot go any farther, cabal repl and :t is the move now
-- see https://hackage.haskell.org/package/prettyprinter-1.7.1/docs/Prettyprinter.html#v:pretty

(<+>) = (P.<+>)
-- using `te = P.pretty` is not working

letkeyword S.Let = P.pretty "let "
letkeyword S.LetRec = P.pretty "letrec"

value (S.Sym s) = P.pretty s
value (S.Int i) = P.pretty i
value (S.Real n) = P.pretty n
value (S.Bool b) = P.pretty(if b then "#t" else "#f")
value S.EmptyList = P.pretty"()"
-- not sure, nr defened group, and seq
value (S.Pair car cdr) = 
    P.group $ P.parens $ P.vsep $ values (S.Pair car cdr)
    where
        values S.EmptyList = []
        values (S.Pair car cdr) = value car : values cdr
        values v = [P.pretty"." <+> value v]

--   fun kw k docs = P.group (te "(" ++ te k ++ te " " ++ P.seq cn id docs ++ te ")")

kw k docs = P.group $ P.parens $ P.pretty k <+> P.vsep docs
wrap = P.group . P.parens . P.vsep


exp (S.Literal v) = case v of
        S.Int _ -> value v
        S.Real _ -> value v
        S.Bool _ -> value v 
        _ -> P.pretty "'" <> value v  
        -- why tik before these value, not shown in intepreter of vschme -vv
exp (S.Var s) = P.pretty s
exp (S.Set x e) = P.nest 3 $ kw "set" [P.pretty x, exp e]
exp (S.IfX e1 e2 e3) = P.nest 3 $ kw "if" [exp e1, exp e2, exp e3]
exp (S.WhileX e1 e2) = P.nest 3 $ kw "while" [exp e1, exp e2]
exp (S.Begin es) = P.group $ P.nest 3 $ P.parens $ P.pretty"begin" <+> P.vsep (map exp es)
exp (S.Apply e es) = P.nest 3 $ wrap (exp e : map exp es)
exp (S.LetX lk bs e) = case lk of
    S.Let -> P.nest 3 $ pplet "let" bindings (exp e)
    S.LetRec -> P.nest 3 $ pplet "letrec" bindings (exp e)
    where
        pplet k bs e = P.parens $ P.pretty k <+> (P.parens $ P.align (P.vsep bs)) <> P.line <> e
        bindings = [P.pretty "[" <> P.pretty x <+> exp e <> P.pretty "]" | (x, e) <- bs]
-- ignore other letkinds because i dont quite get what wppscheme is trying to do
exp (S.Lambda xs body) = P.nest 3 $ kw "lambda" [wrap (map P.pretty xs), exp body]

def (S.Val x e) = P.nest 3 $ kw "val" [P.pretty x, exp e]
def (S.Define x xs e) = P.nest 3 $ kw "define" [P.pretty x, wrap (map P.pretty xs), exp e]
def (S.CheckExpect e1 e2) = P.nest 3 $ kw "check-expect" [exp e1, exp e2]
def (S.CheckAssert e) = P.nest 3 $ kw "check-assert" [exp e]
def (S.Exp e) = exp e

pp = show . def
ppexp = show . exp

-- need to strip final new line?

-- expString = show . ppexp

-- needa figure out where let* comes from

--  test for let beding
-- VU.exp (V.LetX V.Let [("x", (V.Literal (V.Int 1))), ("y", (V.Literal (V.Int 1))) ] (V.Literal (V.Int 1)))