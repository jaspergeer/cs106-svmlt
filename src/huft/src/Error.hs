{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Error where

newtype Error a = Error { runError :: Either String a}
  deriving (Functor, Applicative, Monad)