-- KNormalizer from FO Scheme to KNF
-- this is where register allocation a happens

module KNormalize where

import Prelude hiding ( exp )

import qualified KNF as K
import qualified ClScheme as C
import qualified Env as E
import qualified Error
import qualified Primitives as P
import qualified ObjectCode as O

import qualified Case
import qualified Pattern

import qualified MatchCompiler as MC
import qualified MatchViz as MV
import qualified CSUtil
import qualified Constructed as CONS
-- import qualified VSchemeUnparse as VU

import AsmUtils (i)
-- needa somehow figure out MatchViz here


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

exp :: E.Env Reg -> RegSet -> C.Exp -> Exp
exp rho a e =
  let nbRegs = nbRegsWith (exp rho)
  in case e of
    (C.PrimCall p es) -> nbRegs bindAnyReg a es (K.VMOP p)
    (C.Literal x) -> K.Literal x
    (C.Local n) -> case E.find n rho of
      Error.Error (Left s) -> error s
      Error.Error (Right t) -> K.Name t
    (C.SetLocal n e) -> case E.find n rho of
      Error.Error (Left s) -> error s
      Error.Error (Right t) -> K.Assign t (exp rho a e)

    (C.Global n) -> K.VMOPGLO P.getglobal [] (O.String n)
    (C.SetGlobal n e) -> bindAnyReg a (exp rho a e)
      (\t -> K.Seq
        (K.VMOPGLO P.setglobal [t] (O.String n))
        (K.Name t))
    (C.Begin []) -> K.Literal $ O.Bool False
    (C.Begin es) ->
      let mkSequence [e] = exp rho a e
          mkSequence (e:es) = K.Seq (exp rho a e) (mkSequence es)
      in mkSequence es
    (C.IfX e1 e2 e3) -> bindAnyReg a (exp rho a e1)
      (\t ->  K.If t (exp rho a e2) (exp rho a e3))
    (C.WhileX e1 e2) ->
      let t = smallest a
      in K.While t (exp rho a e1) (exp rho a e2)
    (C.FunCall fun args) ->
      bindSmallest a (exp rho a fun)
        (\t -> nbRegs bindSmallest (a \\ t) args
          (\ts -> K.FunCall t ts))
    (C.Let bindings body) ->
         let (ns, es) = unzip bindings
             bind_regs ns ts = foldl (\rho (n, t) -> E.bind n t rho) rho (zip ns ts)
         in nbRegs bindSmallest a es
              (\ts -> exp (bind_regs ns ts) (foldl (\\) a ts) body)
    (C.ClosureX (C.Closure formals body [])) ->
      let
        -- update funreg by always binding smallest to the formals
        -- maybe can rewrite with bindSmallestReg policy?
        -- NEED TO CHECK the justification of the `funcode` helper function
        (args, funbody) = funcode (C.FunCode formals body)
      in K.FunCode args funbody
    (C.ClosureX (C.Closure formals body captured)) ->
      let
        -- if the captured vars is not empty, they must be put in regs by nbRegsWith
      in
        nbRegs bindAnyReg a captured (\ts ->
        let (args, funbody) = funcode (C.FunCode formals body)
        in K.ClosureX (K.Closure args funbody ts))

    (C.Captured i) -> K.Captured i
    (C.LetRec bindings body) ->
      let
        (a', bindings') = foldr (\(n, _) (a, binds) ->
          let t = smallest a
          in (a \\ t, (n, t) : binds)) (a, []) bindings
        (_, ts) = unzip bindings'
        rho' = foldr (\(n,t) rho -> E.bind n t rho) rho bindings'
        closure :: C.Closure -> (K.Closure Reg -> Exp) -> Exp
        closure (C.Closure formals body captured) k =
          nbRegsWith (exp rho') bindAnyReg a' captured
          (\ts -> k
            (let (args, funbody) = funcode (C.FunCode formals body)
             in K.Closure args funbody ts))
        map' f' [] k = k []
        map' f' (x:xs) k =
          f' x (\y -> map' f' xs (\ys -> k (y:ys)))
      in map' (closure . snd) bindings
        (\cs -> K.LetRec (zip ts cs) (exp rho' a' body))
    (C.Constructed (CONS.T "#t" [])) -> K.Literal (O.Bool True)
    (C.Constructed (CONS.T "#f" [])) -> K.Literal (O.Bool False)
    (C.Constructed (CONS.T "cons" [x, y])) -> nbRegs bindAnyReg a [x, y] (K.VMOP P.cons)
    (C.Constructed (CONS.T "'()" [])) -> K.Literal O.EmptyList
    (C.Constructed (CONS.T cons es)) -> nbRegs bindAnyReg a (C.Literal (O.String cons) : es) K.Block
    (C.Case (Case.T e choices)) -> bindAnyReg a (exp rho a e)
     (\t ->
      let
          treeGen :: RegSet -> MC.Tree C.Exp -> K.Exp Reg
          treeGen a (MC.Test r edgeList (Just defalt)) = K.SwitchVCon r (fmap (\(MC.E c t) -> (c, treeGen a t)) edgeList) (treeGen a defalt)
          treeGen a (MC.LetChild (r, i)  k) = bindAnyReg a (K.VMOPGLO P.getblockslot [r] (O.Int i))
                                                           (\t -> treeGen (a \\ t) (k t))
          treeGen a (MC.Match e env) = exp (E.union env rho) a e -- dont you add union rho and env?
          a' = a \\ t
       in treeGen a' (id (MC.decisionTree t choices))) -- import VUScheme latter for HLS to work



-- work as identity function, might do unsafe IO for side effect
-- vizTree = MV.viz (VU.ppexp . CSUtil.embedExp)

{-
| F.CASE (e, choices) => 
   (... normalize e into a register, using the following continuation ...) (fn t =>
     let fun treeGen = ...
         val _ = treeGen : regset -> F.exp MC.tree -> reg K.exp
         val A' = ... available registers minus t ...
     in  treeGen A' (vizTree (MC.decisionTree (t, choices)))
     end)
The CASE form is defined in structure Case in file case.sml; 
variable e has type F.exp and variable choices has type 
(Pattern.pattern * F.exp) list.

Function vizTree acts like an identity function, except it may also write a visualiation to disk.

Define function treeGen.
-}




{- This function does almost the same thing as the code you have written 
   that K‑normalizes the F.DEFINE form: 
   the formal parameters go into an environment,
    and every nonzero register that is not used to hold a formal parameter 
   is available. The only difference from F.DEFINE is that in a 
   lambda expression, the function has no name,  
   so the environment does not bind a function name to register 0. -}

funcode :: C.FunCode -> K.Funcode Reg
funcode (C.FunCode formals body) =
  let
      args = [1..length formals]
      (funenv, regset) = foldl (\(rho, a) n ->
                (E.bind n (smallest a) rho, a \\ smallest a)) (E.empty, RS 1) formals
      funbody = exp funenv regset body
  in (args, funbody)

-- primcall :: P.Primitive -> [C.Exp] -> Exp
-- primcall p es = exp E.empty (RS 0) (C.PrimCall p es)

def :: C.Def -> Exp
def e = case e of
    (C.Exp e) -> exp E.empty (RS 0) e
    (C.CheckExpect s1 e1 s2 e2) -> K.Seq
      (bindAnyReg (RS 0) (exp E.empty (RS 0) e1)
        (\t -> K.VMOPGLO P.check [t] (O.String s1)))
      (bindAnyReg (RS 0) (exp E.empty (RS 0) e2)
        (\t -> K.VMOPGLO P.expect [t] (O.String s2)))
    (C.CheckAssert s e) ->
      bindAnyReg (RS 0) (exp E.empty (RS 0) e)
        (\t -> K.VMOPGLO P.checkAssert [t] (O.String s))
    (C.Val n e) -> bindAnyReg (RS 0) (exp E.empty (RS 0) e)
      (\t -> K.Seq
        (K.VMOPGLO P.setglobal [t] (O.String n))
        (K.Literal $ O.String n))
    (C.Define funname (C.FunCode formals body)) ->
      let
        args = [1..length formals]
        (funenv, regset) = foldl (\(env, rs) n ->
          let t = smallest rs
              rs' = rs \\ t
          in (E.bind n t env, rs')) (E.bind funname 0 E.empty, RS 1) formals
      in bindAnyReg (RS 0) (K.FunCode args (exp funenv regset body))
          (\t -> (K.setglobal funname t))

    -- (C.Define funname params body) ->
    --   let
    --     (funenv, regset) = foldl (\(env, rs) n ->
    --       let t = smallest rs
    --           rs' = rs \\ t
    --       in (E.bind n t env, rs')) (E.bind funname 0 E.empty, RS 1) params
    --   in K.Let 0 (K.FunCode [1..(length params)] (exp funenv regset body))
    --              (K.VMOPGLO P.setglobal [0] (O.String funname))


-- bindSmallest behaves just like bindAnyReg, except it doesn’t 
-- optimize for the case of an expression already in a register. 
bindSmallest :: RegSet -> Exp -> (Reg -> Exp) -> Exp
bindSmallest a e k = let t = smallest a
                      in K.Let t e (k t)

bindAnyReg :: RegSet -> Exp -> (Reg -> Exp) -> Exp
bindAnyReg a e k = case e of
  (K.Name n) -> k n
  _ -> bindSmallest a e k


{-  printing pattern matching tree   -}

-- structure MC = MatchCompiler(type register = int
--                              fun regString r = "$r" ^ Int.toString r
--                             )

-- structure MV = MatchViz(structure Tree = MC)
-- val vizTree = MV.viz (WppScheme.expString o CSUtil.embedExp)
