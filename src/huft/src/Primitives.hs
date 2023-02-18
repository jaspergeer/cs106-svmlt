module Primitives where

import qualified Data.Map as M

type Name = String

data Base = Base String Int

data Primitive = SetsRegister Base
               | HasEffect Base

binary = [ "+", "-", "*", "/", "<", ">", "cons", "=", "idiv", "Array.sub"
         , "sin", "cos", "tan", "asin", "acos", "atan", "Array.new"
         , "mkclosure"
         ]

unary = [ "boolean?", "null?", "number?", "pair?", "function?", "nil?"
        , "symbol?", "car", "cdr"
        , "Array.new'", "sqrt", "exp", "ln", "Array.length"
        , "!", "ref"
        ]

side_effecty   = [ "print", "printu", "println" ]
errory         = [ "error" ]
halty          = [ "halt" ]
checky         = [ "check", "expect" ]

base p = case p of
  SetsRegister b -> b
  HasEffect b -> b 

name p = case base p of (Base n _) -> n
arity p = case base p of (Base _ a) -> a

throwsError p = name p == "error" || name p == "halt" 

-- (* For SETS_REGISTER, the instruction takes the destination register
--    as the first operand, and the actual parameters as remaining operands.

--    For HAS_FFECT, the instruction has no destination register;
--    the operands are the arguments.
    
--    A SETS_REGISTER primitive _must not_ have a side effect,
--    because if one appears in an effectful context, it is discarded.
--  *)

add :: Int -> (Base -> Primitive) -> [Name] -> [Primitive] -> [Primitive]
add arity ty names prims =
  foldl (\prims name -> ty (Base name arity) : prims)
    prims names

primitives :: [Primitive]
primitives =
    ( add 2 SetsRegister binary
    . add 1 SetsRegister unary
    . add 1 HasEffect side_effecty
    . add 1 HasEffect errory
    . add 2 HasEffect checky
    . add 0 HasEffect halty
    ) [ HasEffect (Base "Array.update" 3)
      , HasEffect (Base "set-car!" 2)
      , HasEffect (Base ":=" 2)
      ]

primMap :: M.Map Name Primitive
primMap = foldr (\p m -> M.insert (name p) p m) M.empty primitives

exposedNames :: [Name]
exposedNames = map name primitives

find :: Name -> Maybe Primitive
find x = M.lookup x primMap

cons         = SetsRegister (Base "cons" 2)
setglobal    = HasEffect (Base "setglobal" 2)
getglobal    = SetsRegister (Base "getglobal" 1)
check        = HasEffect (Base "check" 2)
expect       = HasEffect (Base "expect" 2)
check_assert = HasEffect (Base "check-assert" 2)
loadliteral  = SetsRegister (Base "loadliteral" 1)
mkclosure    = SetsRegister (Base "mkclosure" 2)
setclslot    = HasEffect (Base "setclslot" 3)
getclslot    = SetsRegister (Base "getclslot" 2)