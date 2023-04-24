
-- the MatchCompiler functor is called in knormalize
-- Need to rename Decision Tree to MatchCompiler for haskell module requirement
-- module DecisionTree where
module MatchCompiler where

import qualified Pattern as P
-- no need for ListUtil
import qualified Env as E
import qualified Data.List as L

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
          deriving Eq
-- in order to match block slots, children should be numbered from 1

type Constraint = (Path, Pat)
--  (π, p) is satisfied if the subject subtree at path π matches p

newtype Frontier a = F (a, [Constraint])
{-
    A frontier holds a set of constraints that apply to the scrutinee.

    A choice's initial frontier has just one contraint: [(root, p)],
    where root is the scrutinee register and p is the original pattern
    in the source code.

    A choice is known to match the scrutinee if its frontier
    contains only constraints of the form (REGISTER t, VAR x).
    These constraints show in which register each bound name is stored.

    The key operation on frontiers is *refinement* (called `project`
    in the paper).  Refing revises the constraints under the assumption
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

-- maybeConstructed (π, p) 
--           = SOME (π, vcon, pats), when p is equivalent to P.APPLY (vcon, pats)
--           = NONE                  otherwise
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
-- returns the refinement of the given frontier, if compatible
refineFrontier = undefined

decisionTree :: Register -> [(Pat, a)] -> Tree a
-- register argument is the register that will hold the value of the scrutinee
decisionTree = undefined
