{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Error where


newtype Error a = ERROR { getError :: Either String a}
  deriving (Functor, Applicative, Monad)