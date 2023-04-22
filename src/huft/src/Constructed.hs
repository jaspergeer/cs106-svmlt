-- introduction form for constructed data
module Constructed where

import qualified Pattern as P
import qualified Data.Set as Set

type VCon = P.VCon
type Name = P.Name
data T exp = T VCon [exp] deriving Show

instance Foldable T where
  -- foldr :: (a -> b -> b) -> b -> T a -> b
  foldr f z (T con es) = foldr f z es

instance Functor T where
  fmap f (T con es) = T con (map f es)

free subFree (T con es) = Set.unions (map subFree es)
