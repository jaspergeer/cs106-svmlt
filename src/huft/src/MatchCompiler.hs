
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


-- refineFrontier :: Register -> LabeledConstructor -> Frontier a -> Maybe (Frontier a)
-- -- returns the refinement of the given frontier, if compatible
-- -- I assume r ~ pi / c' ~ C / i ~ i / constraints ~ f
-- -- I hope this is what the 
-- refineFrontier r lcon@(con, arity) frontier@(F (i, constraints)) =
--   case patternAt (REGISTER r) frontier of
--     Just (P.Apply vcon ps) | con == vcon && length ps == arity
--       -> let newcon = concat $ mapCompatible (refineConstraint r lcon) constraints
--           -- what if newcon is INCOMPATIBLE?
--           in Just $ F (i, newcon)
--     Just _ -> Nothing
--     _ -> Just frontier

refineFrontier :: Register -> LabeledConstructor -> Frontier a -> Maybe (Frontier a)
refineFrontier r lcon@(con, arity) frontier@(F (i, constraints)) =
  case patternAt (REGISTER r) frontier of
    Just (P.Apply vcon ps) | con == vcon && length ps == arity
      -> let newcon = concat $ mapCompatible (refineConstraint r lcon) constraints
          -- what if newcon is INCOMPATIBLE?
          in Just $ F (i, newcon)
    Just _ -> Nothing
    _ -> Just frontier

-- refineFrontier r lcon@(con, arity) frontier@(F (i, constraints)) = 
--   case patternAt (REGISTER r) frontier of
--     Nothing -> Nothing
--     Just (P.Var _) -> Just frontier
--     Just P.Wildcard -> Just frontier
--     Just (P.Apply vcon ps) | con == vcon && length ps == arity ->
--       let allcomp = compatibilityConcat (map (refineConstraint r lcon) constraints)
--       in case allcomp of
--         INCOMPATIBLE -> Nothing
--         COMPATIBLE newpairs -> Just $ F (i, newpairs)

decisionTree :: Register -> [(Pat, a)] -> Tree a
-- register argument is the register that will hold the value of the scrutinee
decisionTree r choices = let
  -- initFrontiers = map (\(pat, a) -> F (a, [(REGISTER r, pat)])) choices
  decisionTree' frontiers =
    let
        frontierMatches (F (_, constraints)) =
          not (any (\(_, constraint) -> case constraint of
                      P.Apply {} -> True
                      _ -> False) constraints)
        -- helper function for (MATCH (hd frontiers) on paper)
        match (F (a, constraints)) = Match a (foldr (\(pi, pat) env -> case (pi, pat) of
                                                      (REGISTER r, P.Var x) -> E.bind x r env
                                                      _ -> env) E.empty constraints)
        compile [] = error "no frontiers"
        compile frontiers@(front@(F (a, constraints)):_) =
          if frontierMatches front
          then match front
          else
            let
              dom constraints = [pi' | (pi', _) <- constraints]
              pis = [pi | frontier@(F (_, constraints)) <- frontiers, pi <- dom constraints,
                case patternAt pi frontier of
                  Just (P.Apply {}) -> True
                  _ -> False]
              pi = head pis
            in
              case pi of
                (CHILD (r, i)) -> LetChild (r, i)
                  (\r -> let
                    renamePaths :: [Constraint] -> [Constraint]
                    renamePaths = map (\(pi', pat) -> if pi' == pi then (REGISTER r, pat) else (pi', pat))
                    frontiers' = map (\(F (a, constraints)) -> F (a, renamePaths constraints)) frontiers
                    in compile frontiers')
                REGISTER r' ->
                  let
                    cs = [(cons, length pats) | (_, P.Apply cons pats) <- constraints]
                    refineFrontiers reg lcons =
                      foldr (\ft fts -> case refineFrontier reg lcons ft of
                                        Just ft' -> ft':fts
                                        _ -> fts) []
                    edges = foldr (\lcons edges ->
                      let
                        refined = refineFrontiers r' lcons frontiers
                      in
                        if null refined then edges
                        else E lcons (compile refined) : edges) [] cs

                    defaults = filter (\ft@(F (i, f)) -> notElem pi (dom f) ||
                      case patternAt pi ft of
                        Just (P.Var _) -> True
                        _ -> False) frontiers
                  in Test r' edges (if null defaults then Nothing else Just (compile defaults))
    in compile frontiers
  in foldl (\t@(Test r edges dfault) (pat, a) ->
    case dfault of
      Just _ -> t
      Nothing ->
        case pat of
          P.Wildcard -> Test r edges (Just (Match a E.empty))
          P.Var x -> Match a (E.bind x r E.empty)
          P.Apply vcon as ->
            let
              initFrontier = [F (a, [(REGISTER r, pat)])]
              t' = decisionTree' initFrontier
            in case t' of
              Test _ edges' _ -> Test r (edges ++ edges') Nothing
              _ -> error (show (vcon, as))
        ) (Test r [] Nothing) choices

{-
  Now implement function decisionTree. The TEST and MATCH nodes are described in the paper. 
  When your match compiler produces a node of the form LET_CHILD ((r, i), k), 
  you define continuation k. The continuation expects a new, temporary register, 
  and it updates all the frontiers, substituting the new register for the 
  old path CHILD (r, i). Perform the substitution using function forPath.
-}