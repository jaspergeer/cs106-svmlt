(* Properties of primitives (SVM instructions) *)

(* In module 6, you'll need to extend the implementation to
   account for any exciting SVM instructions you may have defined.
   You'll see here NR's allergy to long stretches of repetitive code.
 *)

structure Primitive :> sig
  (* A primitive corresponds to an SVM instruction *)

  type base
  datatype primitive = SETS_REGISTER of base | HAS_EFFECT of base

    (* For SETS_REGISTER, the instruction takes the destination register
       as the first operand, and the actual parameters as remaining operands.

       For HAS_EFFECT, the instruction has no destination register;
       the operands are the arguments.
       
       A SETS_REGISTER primitive _must not_ have a side effect,
       because if one appears in an effectful context, it is discarded.
     *)

  (* these are the primitives that are available to source code *)
  val find : string -> primitive option  (* intended only for primitives that
                                            appear in source code; internal
                                            compiler primitives should be named
                                            using the values below *)
  val exposedNames : string list


  (* observers for properties of primitives *)
  val name  : primitive -> string   (* used in assembly code & object code *)
  val arity : primitive -> int      (* used to make LAMBDAs on demand *)
  val throwsError : primitive -> bool
     (* primitive does not return, so is OK to use in a value context *)

  (* these are the primitives that are used inside the compiler *)
  val cons         : primitive   (* for building quoted lists *)
  val loadliteral  : primitive
  val setglobal    : primitive
  val getglobal    : primitive
  val error        : primitive   (* used when finding a fault in user code *)
  val check        : primitive   (* for converting check-expect to K-normal form *)
  val expect       : primitive   (* for converting check-expect to K-normal form *)
  val check_assert : primitive
  val mkclosure    : primitive
  val setclslot    : primitive
  val getclslot    : primitive
  val getblockslot : primitive
end
  =
struct

  (* Pure, register-setting primitives grouped by arity.  You can extend these lists *)

  val binary  = [ "+", "-", "*", "/", "<", ">", "cons", "=", "idiv"
                , "mkclosure"
                ]
  val unary   = [ "boolean?", "null?", "number?", "pair?", "function?", "nil?"
                , "symbol?", "car", "cdr"
                ]


  (* Four different groups of side-effecting primitives.  To the compiler,
     `error` looks a lot like `print`, but only `error` throws an error,
     so I feel compelled to separate them. *)

  val side_effecting = [ "print", "printu", "println" ]   (* arity 1 *)
  val error          = [ "error" ]            (* arity 1; never returns *)
  val halt           = [ "halt" ]
  val checky         = [ "check", "expect" ]  (* arity 2; one is literal *)
    (* check and expect can't really be used in source code... *)

  (* Representation of a primitive, with observers *)

  type base = { name : string, arity : int }
  datatype primitive = SETS_REGISTER of base | HAS_EFFECT of base

  fun base (SETS_REGISTER b) = b
    | base (HAS_EFFECT    b) = b

  val name  = #name  o base
  val arity = #arity o base

  fun throwsError p = #name (base p) = "error" orelse #name (base p) = "halt"

  (* building and using the list of primitives *)

  fun add arity ty names prims =
    foldl (fn (name, prims) => ty { name = name, arity = arity } :: prims)
          prims names

  val primitives : primitive list =  (* you can also extend this definition *)
    ( add 2 SETS_REGISTER binary
    o add 1 SETS_REGISTER unary
    o add 1 HAS_EFFECT side_effecting
    o add 1 HAS_EFFECT error
    o add 0 HAS_EFFECT halt
    o add 2 HAS_EFFECT checky
    ) [ (* useful spot to add more effectful primitives *) 
      ]

  val exposedNames = map name primitives
  
  fun find x = List.find (fn p => name p = x) primitives


  (* Primitives used internally *)

  val cons         = SETS_REGISTER { name = "cons",         arity = 2 }
  val setglobal    = HAS_EFFECT    { name = "setglobal",    arity = 2 }
  val getglobal    = SETS_REGISTER { name = "getglobal",    arity = 1 }
  val error        = HAS_EFFECT    { name = "error",        arity = 1 }
  val check        = HAS_EFFECT    { name = "check",        arity = 2 }
  val expect       = HAS_EFFECT    { name = "expect",       arity = 2 }
  val check_assert = HAS_EFFECT    { name = "check-assert", arity = 2 }
  val loadliteral  = SETS_REGISTER { name = "loadliteral",  arity = 1 }
  val mkclosure    = SETS_REGISTER { name = "mkclosure",    arity = 2 }
  val setclslot    = HAS_EFFECT    { name = "setclslot",    arity = 3 }
  val getclslot    = SETS_REGISTER { name = "getclslot",    arity = 2 }
  val getblockslot = SETS_REGISTER { name = "getblockslot", arity = 2 }
end



