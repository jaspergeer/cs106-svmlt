module MatchViz where

import qualified MatchCompiler as MC

-- import qualified Data.GraphViz as GV

import System.Environment

type Tree = MC.Tree

eprint :: String -> IO ()
eprint = putStrLn

outpath :: IO String
outpath = getEnv "MATCHVIZ"


type Attributes = [(String, String)]

quote s = "\"" ++ s ++ "\""
bracket s = "<" ++ s ++ ">"

keyval (key, v) = concat [key, "=", quote v]

render [] = ""
-- render attributes = bracket (concatWith "," (map keyval attributes))


viz :: (a -> String) -> Tree a -> Tree a
viz = undefined
