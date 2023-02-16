{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Error where


newtype Error a = ERROR { runError :: Either String a}
  deriving (Functor, Applicative, Monad)