module MatchViz where

import qualified MatchCompiler as MC
import Data.List
import Control.Monad.Trans.State (State, get, put)

-- import qualified Data.GraphViz as GV

type Tree = MC.Tree

type Attributes = [(String, String)]

quote s = "\"" ++ s ++ "\""
bracket s = "[" ++ s ++ "]"

keyval (key, v) = concat [key, "=", quote v]

render [] = ""
render attributes = bracket (intercalate "," (fmap keyval attributes))

label s = [("label", s)]

type Label a = State Int a

nextNode :: a -> Label String
nextNode _ = do
    l <- get
    put (l + 1)
    l' <- get
    return ("N" ++ show l)

-- node attributes =
--     let name = nextNode ()
--         hi    = undefined [" ", name, " ", render attributes, "\n"]
--     in name
         

viz :: (a -> String) -> Tree a -> Tree a
viz = undefined
