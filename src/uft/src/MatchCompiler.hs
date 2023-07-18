-- taken directly from Norman Ramsey's SML match compiler implementation
module MatchCompiler where

import qualified Pattern as P

-- no need for ListUtil

import Asm (Label)
import qualified Data.List as L
import Data.Maybe
import qualified Data.Sequence as Env
import Debug.Trace
import qualified Env as E
import Control.Monad (join)

-- basic data sturctures

type Register = Int
type Arity = Int
type LabeledConstructor = (String, Arity)
type Pat = P.Pat

data Edge a = E LabeledConstructor (Tree a)
data Tree a
  = Test Register [Edge a] (Maybe (Tree a))
  | LetChild (Register, Int) (Register -> Tree a)
  | Match a (E.Env Register)

data Path
  = REGISTER Register
  | CHILD (Register, Int)
  deriving (Eq)

instance Show Path where
  show (REGISTER r) = "$r" ++ show r
  show (CHILD (r, i)) = "$r" ++ show r ++ "[" ++ show i ++ "]"

instance (Show a) => Show (Edge a) where
  show (E lcon tree) = "[--" ++ show lcon ++ "-->]" ++ show tree

instance (Show a) => Show (Tree a) where
  show (Test r edges defalt) = "[TEST " ++ show r ++ "[EDGE-START" ++ show edges ++ "EDGE-END]" ++ "[otherwize" ++ show defalt ++ "]" ++ "END-TEST]"
  show (Match a env) = "[MATCH," ++ show a ++ show env ++ " END MATCH]"
  show (LetChild (block, i) k) = "[let " ++ show (k 1) ++ " := " ++ "$r3[" ++ show i ++ "]]"

-- in order to match block slots, children should be numbered from 1

type Constraint = (Path, Pat)

--  (π, p) is satisfied if the subject subtree at path π matches p

newtype Frontier a = F (a, [Constraint])

instance Show (Frontier a) where
  show (F (_, cs)) = show (map constraintString cs)

constraintString (pi, p) = show p ++ " @ " ++ show pi

{-
    A frontier holds a set of constraints that apply to the scrutinee.

    A choice's initial frontier has just one contraint: [(root, p)],
    where root is the scrutinee register and p is the original pattern
    in the source code.

    A choice is known to match the scrutinee if its frontier
    contains only constraints of the form (REGISTER t, VAR x).
    These constraints show in which register each bound name is stored.

    The key operation on frontiers is *refinement* (called `project`
    in the paper).  Refining revises the constraints under the assumption
    that a given register holds an application of a given labeled_constructor
-}

data Compatibility a = INCOMPATIBLE | COMPATIBLE a

{-
    Any given point in the decision tree represents knowledge
    about the scrutinee.  At that point, a constraint or a frontier
    may be compatible with that knowledge or incompatible
-}

mapCompatible :: (a -> Compatibility b) -> ([a] -> [b])
mapCompatible f =
  foldr
    ( \a bs -> case f a of
        COMPATIBLE b -> b : bs
        INCOMPATIBLE -> bs
    )
    []

compatibilityConcat :: [Compatibility [a]] -> Compatibility [a]
compatibilityConcat =
  foldr
    ( \a b -> case (a, b) of
        (COMPATIBLE xs, COMPATIBLE ys) -> COMPATIBLE (xs ++ ys)
        _ -> INCOMPATIBLE
    )
    (COMPATIBLE [])

--- DEBUGGING SUPPORT

-- I HOPE WE DO NOT NEED TO DO THIS

-- val eprint = IOUtil.output TextIO.stdErr

-- val patString = WppScheme.patString
-- fun pathString (REGISTER r) = regString r
--   | pathString (CHILD (r, i)) = regString r ^ "." ^ Int.toString i

-- fun frontierString (F (_, constraints)) =
--   let fun conString (pi, p) = patString p ^ "@" ^ pathString pi
--   in  String.concatWith " /\\ " (map conString constraints)
--   end

{----------- DIRTY TRICKS -------------}
--  allow integer literals to masquerade as value constructors

maybeConstructed :: Constraint -> Maybe (Path, P.VCon, [Pat])
maybeConstructed (pi, P.Apply vcon pats) = Just (pi, vcon, pats)
maybeConstructed _ = Nothing

{----------- USEFUL OPERATIONS ON PATHS AND FRONTIERS -------------}

{-  Function `patternAt` implements the @ operation from the paper.
     When `frontier` is `(i, f)`, f@π is `patternAt π frontier` -}
patternAt :: Path -> Frontier a -> Maybe Pat
patternAt pi (F (_, pairs)) =
  let pathIs pi (pi', _) = pi == pi'
   in fmap snd (L.find (pathIs pi) pairs)

{- Substitution for paths: `(new forPath old)` returns
   a substitution function -}
forPath :: Path -> Path -> Frontier a -> Frontier a
newPi `forPath` oldPi =
  let constraint c@(pi, pat) = if pi == oldPi then (newPi, pat) else c
   in (\(F (i, constraints)) -> F (i, map constraint constraints))

{--------- MAIN PART (STUDENT'S RESPONSIBILITY) -----------------}

--------- NR's code

notWildCard :: Pat -> Bool
notWildCard P.Wildcard = False
notWildCard _ = True

isRegister :: Path -> Register -> Bool
(REGISTER r) `isRegister` r' = r == r'
(CHILD _) `isRegister` _ = False

argumentsOfIn :: LabeledConstructor -> Pat -> Maybe [Pat]
argumentsOfIn (vcon, k) (P.Apply vcon' pats) =
  if vcon == vcon' && k == length pats
    then Just pats
    else Nothing
argumentsOfIn _ _ = Nothing

refineConstraint :: Register -> LabeledConstructor -> Constraint -> Compatibility [Constraint]
refineConstraint r lcon (pi', pat) =
  case (pi' `isRegister` r, argumentsOfIn lcon pat, pat) of
    (True, Just pats, _) ->
      COMPATIBLE
        ( filter
            (notWildCard . snd)
            (zipWith (\i p -> (CHILD (r, i), p)) [1 ..] pats)
        )
    (True, _, P.Apply{}) -> INCOMPATIBLE
    _ -> COMPATIBLE [(pi', pat)]

refineFrontier :: Register -> LabeledConstructor -> Frontier a -> Maybe (Frontier a)
refineFrontier r con f@(F (rule, pairs)) =
  case compatibilityConcat (map (refineConstraint r con) pairs) of
    INCOMPATIBLE -> Nothing
    COMPATIBLE pairs -> Just $ F (rule, pairs)

match :: Frontier a -> Tree a
match (F (a, constraints)) =
  Match
    a
    ( foldr
        ( \(pi, pat) env ->
            case (pi, pat) of
              (REGISTER r, P.Var x) -> E.bind x r env
              _ -> env
        )
        E.empty
        constraints
    )

asReg :: Path -> (Register -> Tree a) -> Tree a
asReg (REGISTER r) k = k r
asReg (CHILD (r, i)) k = LetChild (r, i) k

registerize :: [Constraint] -> (E.Env Register -> Tree a) -> Tree a
registerize [] k = k E.empty
registerize ((pi, P.Var x) : pairs) k =
  asReg pi (\t -> registerize pairs (\env -> k (E.bind x t env)))
registerize ((_, pat) : _) _ = error $ ("non-VAR" ++ show pat ++ "at MATCH")

{-
  Now implement function decisionTree. The TEST and MATCH nodes are described in the paper.
  When your match compiler produces a node of the form LET_CHILD ((r, i), k),
  you define continuation k. The continuation expects a new, temporary register,
  and it updates all the frontiers, substituting the new register for the
  old path CHILD (r, i). Perform the substitution using function forPath.
-}

decisionTree :: Register -> [(Pat, a)] -> Tree a
decisionTree r choices =
  let frontier (P.Wildcard, e) = F (e, [])
      frontier (pat, e) = F (e, [(REGISTER r, pat)])
   in compile (map frontier choices)

isSome :: Maybe a -> Bool
isSome (Just _) = True
isSome Nothing = False

anyApplication :: Frontier a -> Maybe (Path, P.VCon, [Pat])
anyApplication (F (_, pairs)) = join $ L.find isSome (map maybeConstructed pairs)

compile :: [Frontier a] -> Tree a
compile [] = error "no frontiers"
compile frontiers@(first : _) =
  case anyApplication first of
    Just (CHILD (r, i), _, _) ->
      -- wants a test node; needs a register
      LetChild
        (r, i)
        ( \t ->
            let frontiers' = map (REGISTER t `forPath` CHILD (r, i)) frontiers
             in compile frontiers'
        )
    Just (pi@(REGISTER r), _, _) ->
      -- test node
      let cons = nub (mapPartial (constructorAppliedAt pi) frontiers)
          subtreeAt con = compile (mapPartial (refineFrontier r con) frontiers)
          edges = map (\con -> E con (subtreeAt con)) cons
          defaults = filter (uncontrainedAt pi) frontiers
          defaultTree =
            if null defaults
              then Nothing
              else Just (compile defaults)
       in Test r edges defaultTree
    Nothing ->
      -- match node
      let F (rule, pairs) = first
       in registerize pairs (\env -> Match rule env)

mapPartial :: (a -> Maybe b) -> [a] -> [b]
mapPartial = mapMaybe

nub :: Eq a => [a] -> [a]
nub = L.nub

constructorAppliedAt :: Path -> Frontier a -> Maybe LabeledConstructor
constructorAppliedAt pi frontier =
  case patternAt pi frontier of
    Just (P.Apply con pats) -> Just (con, length pats)
    _ -> Nothing

uncontrainedAt :: Path -> Frontier a -> Bool
uncontrainedAt pi frontier =
  case patternAt pi frontier of
    Nothing -> True
    Just (P.Var _) -> True
    _ -> False
