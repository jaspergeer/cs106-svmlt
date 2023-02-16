module Primitives where

type Name = String

data Base = Base Name Int

data Primitive = SetsRegister Base
               | HasEffect Base