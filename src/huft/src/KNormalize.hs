-- KNormalizer from FO Scheme to KNF
-- this is where register allocation a happens

module KNormalize where

import Prelude hiding ( exp )

import qualified KNF as K
import qualified FOScheme as F
import qualified Env as E
import qualified Error
import qualified Primitives as P
import qualified ObjectCode as O

type Reg = Int

regname :: Reg -> String
regname r = "$r" ++ show r

newtype RegSet = RS Int

smallest :: RegSet -> Reg
smallest (RS i) = i

(\\) :: RegSet -> Reg -> RegSet
(RS i) \\ x = RS $ max i (x + 1)

-- K normalization

type Exp = K.Exp Reg
type Policy = RegSet -> Exp -> (Reg -> Exp) -> Exp
-- puts the expression in an register, continues

type Normalizer a = RegSet -> a -> Exp

nbRegsWith :: Normalizer a -> Policy -> RegSet -> [a] -> ([Reg] -> Exp) -> Exp
nbRegsWith _ _ _ [] k = k []
nbRegsWith normalize p a (e:es) k = p a (normalize a e)
  (\t -> nbRegsWith normalize p (a \\ t) es (\ts -> k (t:ts)))

helper = undefined

-- use the continuation to decrease a
exp :: E.Env Reg -> RegSet -> F.Exp -> Exp
exp rho a e =
  let nbRegs = nbRegsWith (exp rho)
  in case e of
    (F.PrimCall p es) -> nbRegs bindAnyReg a es (K.VMOP p)
    (F.Literal x) -> K.Literal x
    (F.Local n) -> case E.find n rho of
      Error.Error (Left s) -> error s
      Error.Error (Right t) -> K.Name t
    (F.SetLocal n e) -> case E.find n rho of
      Error.Error (Left s) -> error s
      Error.Error (Right t) -> K.Assign t (exp rho a e)

    (F.Global n) -> let t = smallest a -- this ones iffy
      in K.VMOPGLO P.getglobal [t] (O.String n)
    (F.SetGlobal n e) -> bindAnyReg a (exp rho a e)
      (\t -> K.Seq
        (K.VMOPGLO P.setglobal [t] (O.String n))
        (K.Name t))
    (F.Begin []) -> K.Literal $ O.Bool False
    (F.Begin es) -> 
      let mkSequence [e] = exp rho a e
          mkSequence (e:es) = K.Seq (exp rho a e) (mkSequence es)
      in mkSequence es
    (F.IfX e1 e2 e3) -> bindAnyReg a (exp rho a e1)
      (\t ->  K.If t (exp rho a e2) (exp rho a e3))
    (F.WhileX e1 e2) ->
      let t = smallest a
      in K.While t (exp rho a e1) (exp rho a e2)
    (F.FunCall fun args) ->
      bindSmallest a (exp rho a fun)
        (\t -> nbRegs bindSmallest (a \\ t) args
          (\ts -> K.FunCall t ts))
    (F.Let bindings body) ->
         let (ns, es) = unzip bindings
             bind_regs ns ts = foldl (\rho (n, t) -> E.bind n t rho) rho (zip ns ts)
         in nbRegs bindAnyReg a es
              (\ts -> exp (bind_regs ns ts) (foldl (\\) a ts) body)
      -- let mkLet a' [] body = exp rho a' body
      --     mkLet a' ((n, e):bs) body = nbRegs bindAnyReg a' [e] 
      --                          (\[t] -> mkLet (a' \\ t) bs body)
      -- in mkLet a bindings body
      -- nbregs bindAnyReg a [e] (\[t] -> exp rho (a \\ t) body)
    -- _ -> error $ show e

-- Implement let bindings. K‑normalize the F.LET form. 
-- I encourage you to use nbRegs; you can figure out an appropriate policy. 
-- The continuation will have to remove all the bound registers from the available set. 
-- You can implement this operation in a special-purpose recursive function, 
-- or you can use a fold with a “flipped” version of function --.2

-- You will need to use bindSmallest to put the function in the smallest available register, 
-- then K‑normalize the arguments using nbRegs with bindSmallest as the policy. 
--When K-normalizing the arguments, do not overwrite the register that holds the function.



-- primcall :: P.Primitive -> [F.Exp] -> Exp
-- primcall p es = exp E.empty (RS 0) (F.PrimCall p es)

def :: F.Def -> Exp
def e = case e of
    (F.Exp e) -> exp E.empty (RS 0) e
    (F.CheckExpect s1 e1 s2 e2) -> K.Seq
      (bindAnyReg (RS 0) (exp E.empty (RS 0) e1)
        (\t -> K.VMOPGLO P.check [t] (O.String s1)))
      (bindAnyReg (RS 0) (exp E.empty (RS 0) e2)
        (\t -> K.VMOPGLO P.expect [t] (O.String s2)))
    (F.CheckAssert s e) ->
      bindAnyReg (RS 0) (exp E.empty (RS 0) e)
        (\t -> K.VMOPGLO P.checkAssert [t] (O.String s))
    (F.Val n e) -> bindAnyReg (RS 0) (exp E.empty (RS 0) e)
      (\t -> K.Seq
        (K.VMOPGLO P.setglobal [t] (O.String n))
        (K.Literal $ O.String n))
    (F.Define funname params body) ->
      let
        (funenv, regset) = foldr (\n (env, rs) ->
          let t = smallest rs
              rs' = rs \\ t
          in (E.bind n t env, rs')) (E.bind funname 0 E.empty, RS 1) params
      in K.Let 0 (K.FunCode [1..(length params)] (exp funenv regset body))
                 (K.VMOPGLO P.setglobal [0] (O.String funname))

-- bindSmallest behaves just like bindAnyReg, except it doesn’t 
-- optimize for the case of an expression already in a register. 
bindSmallest :: RegSet -> Exp -> (Reg -> Exp) -> Exp
bindSmallest a e k = let t = smallest a 
                      in K.Let t e (k t)

bindAnyReg :: RegSet -> Exp -> (Reg -> Exp) -> Exp
bindAnyReg a e k = case e of
  (K.Name n) -> k n
  _ -> bindSmallest a e k