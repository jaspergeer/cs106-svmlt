
module MatchViz where

import qualified MatchCompiler as MC

import qualified Data.GraphViz as GV



type Tree = MC.Tree

viz :: (a -> String) -> Tree a -> Tree a
viz = undefined

