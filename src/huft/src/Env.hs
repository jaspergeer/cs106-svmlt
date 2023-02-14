module Env where
import qualified Data.Map as M

type Name = String
type Env a = M.Map Name a

find :: Name -> Env a -> Maybe a
find = M.lookup

bind :: Name -> a -> Env a -> Env a
bind = M.insert

binds :: Env a -> Name -> Bool
binds env n = M.member n env

empty :: Env a
empty = M.empty

-- TODO toString?