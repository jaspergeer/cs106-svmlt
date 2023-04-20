(* Interface to assembly language for the convenience of other translations *)

(* You'll use this interface periodically in modules 6 to 10:

    - You'll use the interface to write your code generator
    - You'll implement `copyreg` in a way that works for your SVM
    - In module 8, you'll add support for function calls
    - In module 10, you'll add support for closures 

 *)


structure AsmGen :> sig
  type reg = ObjectCode.reg          
  type instruction = AssemblyCode.instr
  type label

  type vmop = Primitive.primitive      (* identifies a VM operator, like + or / *)
  type literal = ObjectCode.literal    (* we care only about numbers and strings *)

  val newlabel : 'a -> label  (* fresh every time *)

  (********* instructions that can be generated ******)

  (* simple instructions like cons, +, <; destination register on left *)
  val copyreg   : reg -> reg -> instruction   (* assignment/copy/move *)
  val setreg    : reg -> vmop -> reg list -> instruction
  val setregLit : reg -> vmop -> reg list -> literal -> instruction

  (* effects like setglobal, check, and expect *)
  val effect    : vmop -> reg list -> instruction
  val effectLit : vmop -> reg list -> literal -> instruction
    
  (* control-flow instructions *)
  val goto     : label -> instruction
  val ifgoto   : reg -> label -> instruction
  val deflabel : label -> instruction  (* labels the next instruction *)

  (* function loading *)
  val loadfunc : reg -> int -> instruction list -> instruction

  (* special cases of the code above *)
  val loadlit   : reg -> literal -> instruction   (* rX := LIT     *)
  val getglobal : reg -> literal -> instruction   (* rX := global LIT *)
  val setglobal : literal -> reg -> instruction   (* global LIT := rX *)

  (* call instruction assumes fun and argument in consecutive registers *)
  val call : reg -> reg -> reg -> instruction
                     (* dest, fun, last *)
  val areConsecutive : reg list -> bool
  val return : reg -> instruction
  val tailcall :  reg -> reg -> instruction
                     (* fun, last *)


  val mkblock : reg -> reg -> int -> instruction
    (* x := new block with k slots; x[0] := y *)
  val setblockslot : reg -> int -> reg -> instruction
  val getblockslot : reg -> reg -> int -> instruction

  val gotoVcon : reg -> (ObjectCode.literal * int * label) list -> instruction
end
  =
struct
  structure A = AssemblyCode
  structure O = ObjectCode
  structure P = Primitive

  type reg = ObjectCode.reg
  type instruction = AssemblyCode.instr
  type label = string            
  type vmop = Primitive.primitive
  type literal = ObjectCode.literal


  (***** label generation ****)

  local
    val labelsSupplied = ref 0
  in
    fun newlabel _ =
      let val n = 1 + !labelsSupplied
          val () = labelsSupplied := n
      in  "L" ^ (Int.toString n)
      end
  end

  exception InternalError of string
  fun fail s = raise InternalError s

  (***** sanity checking for correct use of primitives ****)

  fun asValue p =
    case p
      of P.HAS_EFFECT _ => fail ("primitive " ^ P.name p ^ " used for value")
       | P.SETS_REGISTER _ => P.name p

  fun asEffect p =
    case p
      of P.SETS_REGISTER _ => fail ("primitive " ^ P.name p ^ " used for effect")
       | P.HAS_EFFECT _ => P.name p


  fun i con arg = A.OBJECT_CODE (con arg)
  fun regs opr rs = i O.REGS (opr, rs)


  fun setreg    dest operator args   = i O.REGS    (asValue operator, dest::args)
  fun setregLit dest operator args v = i O.REGSLIT (asValue operator, dest::args, v)
  fun effect         operator args   = i O.REGS    (asEffect operator, args)
  fun effectLit      operator args v = i O.REGSLIT (asEffect operator, args, v)

  fun goto label = A.GOTO_LABEL label
  fun deflabel l = A.DEFLABEL l
  fun ifgoto reg l = A.IF_GOTO_LABEL (reg, l)

  fun loadfunc r k body = A.LOADFUNC (r, k, body)

  fun loadlit r v = setregLit r P.loadliteral [] v

  fun getglobal dest  name  = setregLit dest P.getglobal []    name
  fun setglobal name  reg   = effectLit      P.setglobal [reg] name
  
  fun copyreg dest src = Impossible.exercise "register-register move"

  fun call dest function lastarg = regs "call" [dest, function, lastarg]
  fun tailcall  function lastarg = regs "tailcall" [function, lastarg]
  fun return r   = i O.REGS ("return", [r])

  val rec areConsecutive : ObjectCode.reg list -> bool
    = fn []  => true
       | [n] => true
       | n :: m :: ms => m = n + 1 andalso areConsecutive (m :: ms)

  fun mkblock      x y k = i O.REGINT ("mkblock", x, y, k)
  fun setblockslot x k y = i O.REGINT ("setblockslot", x, y, k)
  fun getblockslot x y k = i O.REGINT ("getblockslot", x, y, k)
  fun gotoVcon x choices = 
    let fun choice (vcon, arity, label) = (vcon, arity, label)
    in  A.GOTO_VCON (x, map choice choices)
    end

end
