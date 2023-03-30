-- Embedding and projection between disambiguated VScheme and 
-- First-Order Scheme. Note that projection can fail; embedding can not.

-- This code is so boring it will make your eyes bleed.
-- You already understand it.

module FOUtil where

import Prelude hiding ( exp )

import qualified FOScheme as F
import qualified UnambiguousVScheme as X
import qualified Error as E
import qualified KnProject as KNP
import qualified KnEmbed as KNE
import qualified VScheme as C
import qualified Primitives as P

-- nr used local keyword for exp and def
-- I'm just changing it to exp' and def' for embedding

-- projection
exp :: X.Exp -> E.Error F.Exp
exp e = case e of
    X.Literal v -> return $ F.Literal (KNP.value v)
    X.Local x -> return $ F.Local x
    X.Global x -> return $ F.Global x
    X.SetLocal x e -> F.SetLocal x <$> exp e
    X.SetGlobal x e -> F.SetGlobal x <$> exp e
    X.IfX e1 e2 e3 -> F.IfX <$> exp e1 <*> exp e2 <*> exp e3
    X.WhileX e1 e2 -> F.WhileX <$> exp e1 <*> exp e2
    X.Begin es -> F.Begin <$> mapM exp es
    X.FunCall e es -> F.FunCall <$> exp e <*> mapM exp es
    X.PrimCall p es -> F.PrimCall p <$> mapM exp es
    X.LetX X.Let bs body -> F.Let <$> bindings bs <*> exp body
    X.LetX X.LetRec _ _ -> E.Error $ Left "letrec"
    X.Lambda args e -> E.Error $ Left "lambda"
    where bindings = mapM (\(x, e) -> (,) x <$> exp e)

def :: X.Def -> E.Error F.Def
def d = case d of
    X.Val x e -> F.Val x <$> exp e
    X.Exp e -> F.Exp <$> exp e
    X.Define f xs e -> F.Define f xs <$> exp e
    -- not sure how the sml code works here
    X.CheckExpect s e1 s' e' -> F.CheckExpect s <$> exp e1 <*> return s' <*> exp e'
    X.CheckAssert s e -> F.CheckAssert s <$> exp e

project = def

-- embeding
exp' :: F.Exp -> C.Exp
exp' e = case e of
    F.Literal v -> C.Literal (KNE.value v)
    F.Local x -> C.Var x
    F.Global x -> C.Var x
    F.IfX e1 e2 e3 -> C.IfX (exp' e1) (exp' e2) (exp' e3)
    F.PrimCall p es -> C.Apply (C.Var (P.name p)) (map exp' es)
    F.FunCall e es -> C.Apply (exp' e) (map exp' es)
    F.Let bs e -> C.LetX C.Let (map binding bs) (exp' e)
    F.Begin es -> C.Begin (map exp' es)
    F.SetLocal x e -> C.Set x (exp' e)
    F.SetGlobal x e -> C.Set x (exp' e)
    F.WhileX c body -> C.WhileX (exp' c) (exp' body) 
    where binding (x, e) = (x, exp' e)

def' :: F.Def -> C.Def
def' d = case d of
    F.Val x e -> C.Val x (exp' e)
    F.Exp e -> C.Exp (exp' e)
    F.Define f xs e -> C.Define f xs (exp' e)
    F.CheckExpect s e1 s' e' -> C.CheckExpect (exp' e1) (exp' e')
    F.CheckAssert s e -> C.CheckAssert (exp' e)

embed = def'