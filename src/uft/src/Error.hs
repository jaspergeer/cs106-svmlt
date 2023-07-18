{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Error where

newtype Error a = Error { getError :: Either String a}
  deriving (Functor, Applicative, Monad)

list :: [Error a] -> Error [a]
list es =
  let cons (Error (Right x)) (Error (Right xs)) = Error $ Right (x:xs)
      cons (Error (Left msg)) (Error (Left msg2)) = Error $ Left (msg ++ ";\n" ++ msg2)
      cons (Error (Left msg)) _ = Error $ Left msg
      cons _ (Error (Left msg)) = Error $ Left msg
  in foldr cons (Error $ Right []) es
