module Languages where
import Data.Map
import qualified Data.ByteString as Data.Map.Strict

data Language = HOX | HO | FO | CL | KN | VS | VO deriving (Enum, Eq, Show)
instance Ord Language where
  (<=) :: Language -> Language -> Bool
  a <= b = fromEnum a <= fromEnum b

tableInit = 
  [(HOX, "hox", "Higher-order vScheme with mutable variables in closures")
    , (HO,  "ho",  "Higher-order vScheme")
    , (FO,  "fo",  "First-order vScheme")
    , (CL,  "cl",  "First-order vScheme with closure and capture forms")
    , (KN,  "kn",  "K-Normal form")
    , (VS,  "vs",  "VM assembly language")
    , (VO,  "vo",  "VM object code")]

table :: Map Language (String, String)
table = Prelude.foldr (\(x, y, z) m -> insert x (y, z) m)  Data.Map.empty tableInit
  

shortTable :: Map String Language
shortTable = Prelude.foldr (\(x, y, _) m -> insert y x m)  Data.Map.empty tableInit

pred :: Language -> Maybe Language
pred HOX = Nothing
pred l = Just $ Prelude.pred l

find :: String -> Maybe Language
find x = Data.Map.lookup x shortTable

description :: (String, String) -> String
description (_, d) = d