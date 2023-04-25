
-- the MatchCompiler functor is called in knormalize
-- Need to rename Decision Tree to MatchCompiler for haskell module requirement
-- module DecisionTree where
module MatchCompiler where

import qualified Pattern as P
-- no need for ListUtil
import qualified Env as E
import qualified Data.List as L
import Data.Maybe
import qualified Data.Sequence as Env

-- basic data sturctures

type Register = Int
type Arity = Int
type LabeledConstructor = (String, Arity)
type Pat = P.Pat

data Edge a = E LabeledConstructor (Tree a)
data Tree a = Test Register [Edge a] (Maybe (Tree a))
            | LetChild (Register, Int) (Register -> Tree a)
            | Match a (E.Env Register)

data Path = REGISTER Register
          | CHILD (Register, Int)
          deriving (Eq, Show)
-- in order to match block slots, children should be numbered from 1

type Constraint = (Path, Pat)
--  (π, p) is satisfied if the subject subtree at path π matches p

newtype Frontier a = F (a, [Constraint])

instance Show (Frontier a) where
  show (F (_, cs)) = show cs
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
mapCompatible f = foldr (\a bs -> case f a of
                                        COMPATIBLE b -> b:bs
                                        INCOMPATIBLE -> bs)
                                        []

compatibilityConcat :: [Compatibility [a]] -> Compatibility [a]
compatibilityConcat = foldr (\a b -> case (a, b) of
    (COMPATIBLE xs, COMPATIBLE ys) -> COMPATIBLE (xs ++ ys)
    _ -> INCOMPATIBLE)
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
patternAt pi (F (_, pairs)) = let pathIs pi (pi', _) = pi == pi'
                              in fmap snd (L.find (pathIs pi) pairs)

{- Substitution for paths: `(new forPath old)` returns
   a substitution function -}
forPath :: Path -> Path -> Frontier a -> Frontier a
newPi `forPath` oldPi =
  let constraint c@(pi, pat) = if pi == oldPi then (newPi, pat) else c
  in (\(F (i, constraints)) -> F (i, map constraint constraints))


{--------- MAIN PART (STUDENT'S RESPONSIBILITY) -----------------}

refineConstraint :: Register -> LabeledConstructor -> Constraint -> Compatibility [Constraint]
refineConstraint r lcon constraint =
  case (lcon, constraint) of
    (_, (pi', _)) | REGISTER r /= pi' -> COMPATIBLE [constraint]
    ((con, arity), (pi', P.Apply vcon ps)) | con == vcon && length ps == arity
      -> COMPATIBLE $ zipWith (\i p -> (CHILD (r, i), p)) [1..(length ps)] ps
    _ -> INCOMPATIBLE

refineFrontier :: Register -> LabeledConstructor -> Frontier a -> Maybe (Frontier a)

-- refineFrontier r lcon@(con, arity) frontier@(F (i, constraints)) =
--   case patternAt (REGISTER r) frontier of
--     Just (P.Apply vcon ps) | con == vcon && length ps == arity
--       -> let newcon = concat $ mapCompatible (refineConstraint r lcon) constraints
--           -- what if newcon is INCOMPATIBLE?
--           in Just $ F (i, newcon)
--     Just _ -> Nothing
--     _ -> Just frontier

-- refineFrontier r lcon@(con, arity) frontier@(F (i, constraints)) = 
--   case patternAt (REGISTER r) frontier of
--     Nothing -> Nothing
--     -- the following two cases can be reduces
--     Just (P.Var _) -> Just frontier
--     Just P.Wildcard -> Just frontier
--     Just (P.Apply vcon ps) | con == vcon && length ps == arity ->
--       let allcomp = compatibilityConcat (map (refineConstraint r lcon) constraints)
--       in case allcomp of
--         INCOMPATIBLE -> Nothing
--         COMPATIBLE newpairs -> Just $ F (i, newpairs)

refineFrontier r con f@(F (rule, pairs)) =
  case compatibilityConcat (map (refineConstraint r con) pairs) of
    INCOMPATIBLE -> Nothing
    COMPATIBLE newpairs -> Just $ F (rule, newpairs)

match :: Frontier a -> Tree a
match (F (a, constraints)) = Match a (foldr (\(pi, pat) env ->
  case (pi, pat) of
    (REGISTER r, P.Var x) -> E.bind x r env
    _ -> env) E.empty constraints)

compile :: Register -> [Frontier a] -> Tree a
compile scrutinee frontiers@(front@(F (a, constraints)):_) =
  case [pi | (pi, P.Apply {}) <- constraints]
    of [] -> match front
       pi@(CHILD (r, i)) : _ -> LetChild (r, i)
        (\r ->
          let frontiers' = map (REGISTER r `forPath` pi) frontiers
          in compile scrutinee frontiers')
       pi@(REGISTER r) : _ ->
        let
          dom constraints = [pi' | (pi', _) <- constraints]
          cs = mapMaybe (\ft@(F (i, f)) ->
            if pi `elem` dom f then Nothing
            else do
              pat <- patternAt pi ft
              case pat of
                P.Apply cons pats -> Just (cons, length pats)
                _ -> Nothing) frontiers
          refineFrontiers reg lcons = mapMaybe (refineFrontier reg lcons)
          edges = map (\lcons ->
            let
              refined = refineFrontiers scrutinee lcons frontiers
            in E lcons (compile scrutinee refined)) cs
          defaults = filter (\ft@(F (i, f)) -> notElem pi (dom f) ||
            case patternAt pi ft of
              Just (P.Var _) -> True
              _ -> False) frontiers
        in Test scrutinee edges (if null defaults then Nothing else Just (compile scrutinee defaults))

split :: (a -> Bool) -> [a] -> ([a], [a])
split p l =
  let
    split' p as (b:bs) = if p b then split' p (b:as) bs else (as, b:bs)
  in split' p [] l

asReg (REGISTER r) k = k r
asREg (CHILD (r, i)) k = LetChild (r, i) k


registerize [] k = k Env.Empty
registerize ((pi, P.Var x) : pairs) k = 
  asReg pi (\t -> registerize pairs (\env -> E.bind x t)) -- not true
registerize ((_, pat) : _) _ = undefined

{-
  Now implement function decisionTree. The TEST and MATCH nodes are described in the paper. 
  When your match compiler produces a node of the form LET_CHILD ((r, i), k), 
  you define continuation k. The continuation expects a new, temporary register, 
  and it updates all the frontiers, substituting the new register for the 
  old path CHILD (r, i). Perform the substitution using function forPath.
-}

decisionTree :: Register -> [(Pat, a)] -> Tree a
-- register argument is the register that will hold the value of the scrutinee

decisionTree scrutinee choices =
  let
    initFrontiers = map (\(pat, a) -> F (a, [(REGISTER scrutinee, pat)])) choices
  in compile scrutinee initFrontiers
