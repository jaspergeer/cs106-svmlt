(* Part of the Pretty Printer *)

(* You can ignore this *)

structure PPUtil : PP_UTIL = struct
  structure pp = PP.Short
  fun commaSeparate empty =
    let fun c [] = [empty]
	  | c [x] = [x]
          | c (h::t) = h :: PP.TEXT ", " :: PP.Short.on :: c t
    in  PP.LIST o c
    end

  local 
    val pre  = pp.$ " $t${$c"
    val post = pp.$ " $b$c$}"
    val m1 = pp.$ " $b$c"
    val m2 = pp.$ " $t$c"
    fun tail [(e, kw)] = e :: post :: pp.te kw :: nil
      | tail [] = [post]
      | tail ((e, kw)::t) = e :: m1 :: pp.te kw :: m2 :: tail t
  in
    fun interleave(h, t) = pp.li (pp.te h :: pre :: tail t)
  end
  fun mixin _      (open', close, break) [x] = pp.li [pp.be, open', x, close, pp.en]
    | mixin indist (open', close, break) (h::t) =
        pp.li (pp.be :: open' :: h :: pp.i indist ::
	       foldr (fn (e, l) => break :: e :: l) [pp.ou, close, pp.en] t)
    | mixin _ (open', close, _) [] = pp.li [pp.be, open', close, pp.en]

  val open' = pp.te "("
  val close = pp.te ")"
  fun lisp break (f, l) = mixin 2 (open', close, break) (pp.te f :: l)
  val lispo = lisp (pp.$ " $o")     
  val lispc = lisp (pp.$ " $c")     

  val newlines = mixin 0 (pp.te "", pp.te "", pp.nl)

end

