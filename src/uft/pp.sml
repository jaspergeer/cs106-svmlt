(* Part of the Pretty Printer *)

(* You can ignore this *)

structure PP : PP = struct
  datatype break = OPTIONAL | CONNECTED | FORCED
  datatype pretty = BEGIN 
                  | END 
                  | INDENT of int 
                  | OUTDENT 
                  | BREAK of {break: break, pre:string, post:string, none:string}
                  | TEXT of string 
                  | LIST of pretty list
                  | SYNCH
  type break_info = {break: break, pre:string, post:string, none:string}
  structure Short = struct
    val be = BEGIN
    val en = END
    val i  = INDENT
    val ou = OUTDENT
    val nl = BREAK {break=FORCED, pre="", post="", none=""}
    val on = BREAK {break=OPTIONAL, pre="", post="", none=""}
    val cn = BREAK {break=CONNECTED, pre="", post="", none=""}
    val te = TEXT
    val li = LIST
    val sy = SYNCH
    val int = TEXT o Int.toString
    val char = TEXT o Char.toString
    fun dollar percent s inserts =
      let infixr 5 :::
          fun [] ::: l = l
            | r  ::: l = TEXT (String.implode (rev r)) :: l
          fun e(#"$" :: #"{" :: s, r, inserts) =  r ::: be  :: e(s, [], inserts)
            | e(#"$" :: #"}" :: s, r, inserts) =  r ::: en  :: e(s, [], inserts)
            | e(#"$" :: #"t" :: s, r, inserts) =  r ::: i 2 :: e(s, [], inserts)
            | e(#"$" :: #"b" :: s, r, inserts) =  r ::: ou  :: e(s, [], inserts)
            | e(#"$" :: #"n" :: s, r, inserts) =  r ::: nl  :: e(s, [], inserts)
            | e(#"$" :: #"o" :: s, r, inserts) =  r ::: on  :: e(s, [], inserts)
            | e(#"$" :: #"c" :: s, r, inserts) =  r ::: cn  :: e(s, [], inserts)
            | e(#"$" :: #"#" :: s, r, inserts) =  r ::: sy  :: e(s, [], inserts)
            | e(#"$" :: #"$" :: s, r, inserts) = e(s, #"$" :: r, inserts)
            | e(#"$" :: d    :: s, r, inserts) =
                if #"0" <= d andalso d <= #"9" then
                  r ::: i (ord d - ord #"0") :: e(s, [], inserts)
                else
                  r ::: te ("((internal PP bug --- unknown escape $" ^ str d ^ "))")
                     :: e(s, [], inserts)
            | e(#"\n" :: s, r, inserts) = r ::: nl :: e(s, [], inserts)
            | e(#"%"  :: #"s" :: s, r, inserts) = 
                if percent then
                  case inserts
                    of i :: ii => r ::: i :: e(s, [], ii)
                     | [] => r ::: te "((internal bug: bad call to PP \
                                      \[missing %s argument]))" :: e(s, [], [])
                else
                  e(#"s" :: s, #"%" :: r, inserts)
            | e(#"%"  :: c :: s, r, inserts) = 
                if percent then
                  e(s, c :: r, inserts)
                else
                  e(c :: s, #"%" :: r, inserts)  (* N.B. c could be #"$" *)
            | e(c::s, r, inserts) = e(s, c::r, inserts)
            | e([], r, []) = r ::: nil
            | e([], r, inserts) = r ::: te ("((internal bug: bad call to PP -- " ^
                                             Int.toString (length inserts) ^
                                             " leftover %s arguments: ")
                                     :: inserts @ [te "))"]
      in LIST (e (explode s, [], inserts))
      end
    fun $ s = dollar false s []
  end
  val format = Short.dollar true
  fun flatten BEGIN     = ""
    | flatten END       = ""
    | flatten (INDENT _)= ""
    | flatten OUTDENT   = ""
    | flatten (BREAK {break=FORCED, pre, post, none}) = pre ^ "\n" ^ post
    | flatten SYNCH = "\n"
    | flatten (BREAK {break=_, pre, post, none}) = none
    | flatten (TEXT s)  = s
    | flatten (LIST l)  = String.concat(map flatten l)
  val synch = LIST [Short.nl, TEXT "#line 999 \"generated-code\"", Short.nl]
  val synch = SYNCH
  fun textMap sigma = 
    let fun m (TEXT s)  = TEXT (sigma s)
          | m (LIST l)  = LIST (map (textMap sigma) l)
          | m p = p
    in  m
    end
end
