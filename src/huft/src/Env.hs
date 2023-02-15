module Env where
import qualified Data.Map as M
import qualified Error as E

type Name = String
type Env a = M.Map Name a

-- might have a concise clausal definition
find :: Name -> Env a -> E.Error a
find n env = case M.lookup n env of
    Just x -> Right x
    Nothing -> Left ("Name '" ++ n ++ "' not bound")

bind :: Name -> a -> Env a -> Env a
bind = M.insert

binds :: Env a -> Name -> Bool
binds env n = M.member n env

empty :: Env a
empty = M.empty

-- TODO toString?