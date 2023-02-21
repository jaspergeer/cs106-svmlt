{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Error where

newtype Error a = Error { getError :: Either String a}
  deriving (Functor, Applicative, Monad)