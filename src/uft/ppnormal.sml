(* Part of the Pretty Printer *)

(* You can ignore this *)

structure PPNormal = struct
  type indentation = int
  datatype normal = BLOCK of normal list
                  | BREAK of indentation * PP.break_info
                  | TEXT  of string
                  | SYNCH of indentation
  val normalize : PP.pretty -> normal = fn l => let fun ppfold f zero (PP.LIST l) = foldl (fn (p, z) => ppfold f z p) zero l
						      | ppfold f zero p = f(p, zero)
						    type indent = { size : int, stack : int list }
						    val brev = BLOCK o rev
						    fun bad _ = Impossible.impossible "bad prettyprinting" (* will need to improve *)
						    fun errmsg msg = TEXT ("((pp error: " ^ msg ^ "))")
						    fun next(pp, (indent as {size, stack}, waiting, current)) =
						      let fun add i = (indent, waiting, i::current)
						          val addError = add o errmsg
						          fun n (PP.BEGIN)    = (indent, current::waiting, [])
						            | n (PP.END)      = (case waiting of h::t => (indent, t, brev current::h)
						                                               | []   => addError "unmatched end")
						            | n (PP.INDENT n) = ({size=size+n, stack=n::stack}, waiting, current)
						            | n (PP.OUTDENT)  = (case stack
						                                   of n::t => ({size=size-n,stack=t}, waiting, current)
						                                    | [] => addError "unmatched outdent")
						            | n(PP.BREAK b)   = add (BREAK(#size indent, b))
						            | n(PP.SYNCH)     = add (SYNCH (#size indent))
						            | n(PP.TEXT t)    = add (TEXT t)
						            | n(PP.LIST _)    = Impossible.impossible "can't happen -- bad ppfold"
						       in   n pp
						      end
						    val (indent, waiting, current) = ppfold next ({size=0,stack=[]}, [], []) l
						    val current = case indent of {stack=[], ...} => current
						                               | _ => errmsg "unclosed indent" :: current
						    fun matchBegins ([], cur) = cur
						      | matchBegins (h::t, cur) =
						          matchBegins(t, brev cur :: errmsg "unmatched begin" :: h)
						    val current = matchBegins (waiting, current)
						in  brev current
						end
  fun listify normal = 
    let fun flat(TEXT s, tail) = s::tail
          | flat(BREAK (_, {none, ...}), tail) = none::tail
          | flat(BLOCK l, tail) = foldr flat tail l
          | flat(SYNCH _, tail) = tail
    in  flat(normal, [])
    end
end
