module CodeGen where

import qualified ObjectCode as O
import qualified Asm as A
import qualified KNF as K
import qualified Primitives as P
import qualified AsmUtils as U
import Control.Monad.Trans.State (state, evalState)
import qualified MatchCompiler as M
import Data.Char (isDigit)
import Data.Foldable (foldrM, foldlM)

import Debug.Trace

type Reg = O.Reg
type Instruction = A.Instr

----- Join lists, John Hughes (1985) style -----

type HughesList a = [a] -> [a]
type Code = HughesList Instruction
type Exp = K.Exp Reg
type Generator a = Exp -> U.UniqueLabelState a

empty :: HughesList a
empty tail = tail

-- singleton
s :: a -> HughesList a
s e tail = e : tail

-- conversion
l :: [a] -> HughesList a
l es tail = es ++ tail

hconcat :: [HughesList a] -> HughesList a
hconcat = foldr (.) empty

-- I am very proud of this
(<.>) :: (Applicative f) => f (b -> c) -> f (a -> b) -> f (a -> c)
x <.> y = (.) <$> x <*> y

---- the code generator ----

makeIf toX x e1 e2 = do
      l <- U.newLabel
      l' <- U.newLabel
      branch2 <- toX e2
      branch1 <- toX e1
      return $ s (U.ifgoto x l) . branch2 . s (U.goto l') .
               s (U.deflabel l) . branch1 . s (U.deflabel l')

conLiteral :: M.LabeledConstructor -> O.Literal
conLiteral lcons = case lcons of
  ("#t", 0) -> O.Bool True
  ("#f", 0) -> O.Bool False
  ("'()", 0) -> O.EmptyList
  (cons, 0) | all isDigit cons -> O.Int (read cons)
  (cons, _) -> O.String cons

switchVcon :: Generator Code -> Code -> (Reg, [(M.LabeledConstructor, Exp)], Exp) -> U.UniqueLabelState Code
switchVcon gen finish (r, choices, fallthru) = do
  (table, cases) <- foldlM (\(table, cases) ((cons, arity), e) ->
    do 
      l <- U.newLabel
      body <- gen e
      return ((conLiteral (cons, arity), arity, l): table, cases . s (U.deflabel l) . body . finish))
      ([], empty) choices
  dfault <- gen fallthru
  return $ s (A.GotoVCon r table) . cases . dfault

-- Each subexpression is translated using gen, and its translation is followed by (a copy of) finish.

-- Every subexpression except fallthru is preceded by the definition of a fresh label.

-- The code returned by switchVcon starts with an AssemblyCode.GOTO_VCON form,
-- which contains a jump table. That form is followed by the translation of fallthru. 
-- And that translation is followed by the translations of all the other expressions 
-- in any order, each of which is preceded by the definition of its corresponding label.

toReg' :: Reg -> Generator Code
toReg' dest e = trace (show e) $ case e of
    -- forms with direct translation
    K.Literal lit -> return $ s $ U.reglit "loadliteral" dest lit
    K.Name a -> return $ s $ U.copyreg dest a
    K.VMOP prim@(P.SetsRegister _) rs -> return $ s $ U.setReg dest prim rs
    K.VMOP {} -> forEffect' e <.> return (s $ U.reglit "loadliteral" dest O.Nil) -- "undefined behavior"
    K.VMOPGLO prim@(P.SetsRegister _) _ lit -> return $ s $ U.setRegLit dest prim lit -- the [r1] list disappears here, is that right?
    K.VMOPGLO {} -> forEffect' e
    K.FunCall funreg args -> return $ s $ U.call dest funreg args
    K.FunCode args body -> toReg' dest (K.ClosureX (K.Closure args body []))
    -- control flow forms
    K.If x e1 e2 -> makeIf (toReg' dest) x e1 e2
    K.While x e1 e2 -> forEffect' (K.While x e1 e2) <.> toReg' dest (K.Literal $ O.Bool False)
    -- floatable forms
    K.Let x e1 e' -> toReg' x e1 <.> toReg' dest e'
    K.Seq e1 e2 -> forEffect' e1 <.> toReg' dest e2 -- not sure how does this works
    K.Assign x e -> toReg' x e <.> toReg' dest (K.Name x) -- I assume this is copy reg
    K.Captured i -> return $ s $ U.captured dest i
    K.ClosureX (K.Closure args body captured) -> do
      b' <- toReturn' body
      return $ s (A.LoadFunc dest (length args) (b' [])) .
               s (U.mkclosure dest dest (length captured)) .
               l (mapi (U.setclslot dest) captured)
    K.LetRec bindings body -> letrec (toReg' dest) bindings body
    K.Block es ->
      return $ s (U.mkblock dest dest (length es)) .
               l (mapi (U.setblkslot dest) es)
    K.SwitchVCon x cases dfault -> do
      exit <- U.newLabel
      switchVcon (toReg' dest) (s (U.goto exit)) (x, cases, dfault) <.>
        return (s (U.deflabel exit))
    -- _ -> error $ show e
-- Using A.mkclosure, allocate the closure into that register.
-- Initialize the slots by emitting a sequence of instructions created using A.setclslot.

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f xs =
  let go i [] = []
      go i (x:xs) = f i x : go (i+1) xs
  in go 0 xs

forEffect' :: Generator Code
forEffect' e = case e of
  K.Literal lit -> return empty
  K.Name a -> return empty
  -- assume op has side effect
  K.VMOP (P.HasEffect (P.Base op _)) args -> return $ s (A.ObjectCode (O.Regs op args))
  -- otherwise
  K.VMOP _ _ -> return empty
  K.VMOPGLO (P.HasEffect (P.Base op _)) args v -> return $ s $
  {- assume args only have one argument -}
  {- assume args only have one argument -}
  {- assume args only have one argument -}
  {- assume args only have one argument -}
  {- assume args only have one argument -}
  {- assume args only have one argument -}
  {- assume args only have one argument -}
  {- assume args only have one argument -} A.ObjectCode (O.RegLit op (head args) v)
  -- here we did not catch getglo because it is a SetRegister Primitive
  -- the O.RegLit is also worrysome
  K.VMOPGLO {} -> return empty
  K.FunCall funreg _ -> toReg' funreg e -- what register is killed by the call
  K.FunCode args body -> return empty
  K.If x e1 e2 -> makeIf forEffect' x e1 e2
  K.While x e e' -> do
    l <- U.newLabel
    l' <- U.newLabel
    body <- forEffect' e'
    check <- toReg' x e
    return $ s (U.goto l) . s (U.deflabel l') . body .
             s (U.deflabel l) . check . s (U.ifgoto x l')
  K.Let x e e' -> toReg' x e <.> forEffect' e'
  K.Seq e1 e2 -> forEffect' e1 <.> forEffect' e2
  K.Assign x e -> toReg' x e
  -- If a CLOSURE form is evaluated for side effect, it is simply discarded
  K.Captured i -> return empty
  K.ClosureX (K.Closure args body captured) -> return empty
  K.LetRec bindings body -> letrec forEffect' bindings body
  K.Block {} -> return empty
  K.SwitchVCon x cases dfault -> do
      exit <- U.newLabel
      switchVcon forEffect' (s (U.goto exit)) (x, cases, dfault) <.>
        return (s (U.deflabel exit))

-- wont be implementing till module 8
toReturn' :: Generator Code
toReturn' e = let
  returnx x = U.regs "return" [x]
  returnIn0 e = toReg' 0 e <.> return (s $ returnx 0)
  in case e of
  K.Literal lit -> returnIn0 e
  K.Name a -> return $ s $ returnx a
  K.VMOP prim args -> returnIn0 e
  K.VMOPGLO prim args v -> returnIn0 e
  K.FunCall funreg args -> return $ s $ U.tailcall funreg args
  K.FunCode args body -> returnIn0 e
  -- control flow
  K.If x e1 e2 -> do
      l <- U.newLabel
      branch2 <- toReturn' e2
      branch1 <- toReturn' e1
      return $ s (U.ifgoto x l) . branch2 .
               s (U.deflabel l) . branch1
  K.While x e e' -> returnIn0 e
  -- floatable
  K.Seq e1 e2 -> forEffect' e1 <.> toReturn' e2
  K.Let x e e' -> toReg' x e <.> toReturn' e'
  K.Assign _ e -> toReturn' e
  -- not sure?
  K.Captured i -> returnIn0 e
  K.ClosureX (K.Closure args body captured) -> returnIn0 e
  K.LetRec bindings body -> do
    body' <- toReg' 0 body
    letrec toReturn' bindings body <.> return (body' . s (returnx 0))
  K.Block {} -> returnIn0 e
  K.SwitchVCon x cases dfault -> switchVcon toReturn' empty (x, cases, dfault)
  

codeGen :: [Exp] -> [Instruction]
codeGen es = foldr (.) empty (evalState (mapM forEffect' es) 0) []

letrec :: Generator Code -> [(Reg, K.Closure Reg)] -> Generator Code
letrec gen bindings body =
  let alloc (f_i, K.Closure formals body captures) =
        toReg' f_i (K.FunCode formals body) <.> return (s (U.mkclosure f_i f_i (length captures)))
      init (f_i, K.Closure formals body captures) = l (mapi (U.setclslot f_i) captures)
  in foldr (\b l -> alloc b <.> l) (return empty) bindings <.> return (hconcat (map init bindings)) <.> gen body

-- for mapi, you might want to use ZipWith (that have access to an infinite list)

-- fun letrec gen (bindings, body) =
--    let val _ = letrec : (reg K.exp ‑> instruction hughes_list)
--                      ‑> (reg * reg K.closure) list * reg K.exp
--                      ‑> instruction hughes_list
--       (* one helper function to allocate and another to initialize *)
--       fun alloc (f_i, closure as (funcode as (formals, body), captures)) = ...
--       fun init  (f_i, closure as (funcode as (formals, body), captures)) = ...
--   in  hconcat (map alloc bindings) o hconcat (map init bindings) o gen body
--   end
-- and toReg' ...
-- and forEffect' ...
-- and toReturn' ...