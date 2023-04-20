(* Representation of First-Order Scheme *)

(* You'll need to understand what's going on and how it's used *)


structure FirstOrderScheme = struct
  type name = string
  datatype literal = datatype ObjectCode.literal
  datatype exp
    = LITERAL of literal
    | LOCAL     of name
    | GLOBAL    of name
    | SETLOCAL  of name * exp
    | SETGLOBAL of name * exp
    | IFX       of exp * exp * exp
    | WHILEX    of exp * exp
    | BEGIN     of exp list
    | FUNCALL   of exp * exp list
    | PRIMCALL  of Primitive.primitive * exp list
    | LET       of (name * exp) list * exp
    | CASE        of exp Case.t
    | CONSTRUCTED of exp Constructed.t

  datatype def  = VAL    of name * exp
                | DEFINE of name * (name list * exp)
                | EXP    of exp
                | CHECK_EXPECT of string * exp * string * exp
                | CHECK_ASSERT of string * exp
end
