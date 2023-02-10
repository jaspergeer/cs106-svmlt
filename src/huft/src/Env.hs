module Env where
import qualified Data.Map as M
import Control.Exception ( Exception, throw )

type Name = String
type Env a = M.Map Name a

newtype EnvException = NotFound Name

instance Show EnvException where
  show (NotFound n) = "Name " ++ n ++ " not found"
instance Exception EnvException

find :: Name -> Env a -> a
find n rho = case M.lookup n rho of
  Just x -> x
  _ -> throw (NotFound n)

bind :: Name -> a -> Env a -> Env a
bind = M.insert

binds :: Env a -> Name -> Bool
binds env n = M.member n env

empty :: Env a
empty = M.empty

-- TODO toString?