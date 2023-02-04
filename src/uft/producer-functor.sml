(* Full implementation of the producer interface *)

(* You'll need to use this signature (provided after module 3 lab) *)

functor MkListProducer(val species : string  (* identifies the producer *)
                       type input
                       val show : input list -> string
                      ) :> PRODUCER where type input = input
  =
struct
  fun curry f x y = f (x, y)
  fun fst (x, y) = x
  fun snd (x, y) = y
  fun id x = x
  fun pair x y = (x, y)
  fun curry3 f x y z = f (x, y, z)
  fun eq x y = (x = y)

  type input = input
  structure E = Error
  type 'a error = 'a E.error
  
  type 'a producer_fun = input list -> ('a error * input list) option
  type 'a producer = 'a producer_fun
  fun asFunction p = p
  fun ofFunction f = f
  fun transformWith t = t

  fun produce p inputs =
    case p inputs
      of NONE => E.ERROR (species ^ " did not recognize the input")
       | SOME (a, []) => a
       | SOME (a, leftover) =>
           let val outcome =
                 case a
                   of E.ERROR s => " failed with error \"" ^ s ^ "\" at input: "
                    | _ => " succeeded but did not consume this input: "
           in  E.ERROR (species ^ outcome ^ show leftover)
           end

  fun succeed y = fn xs => SOME (E.OK y, xs)

  infix 3 <*>
  fun tx_f <*> tx_b =
    fn xs => case tx_f xs
               of NONE => NONE
                | SOME (E.ERROR msg, xs) => SOME (E.ERROR msg, xs)
                | SOME (E.OK f, xs) =>
                    case tx_b xs
                      of NONE => NONE
                       | SOME (E.ERROR msg, xs) => SOME (E.ERROR msg, xs)
                       | SOME (E.OK y, xs) => SOME (E.OK (f y), xs)

  infixr 4 <$>
  fun f <$> p = succeed f <*> p

  infix 1 <|>
  fun t1 <|> t2 = (fn xs => case t1 xs of SOME y => SOME y | NONE => t2 xs) 

  fun pzero _ = NONE
  fun perror msg ts = SOME (E.ERROR msg, ts)

  infix 6 <~> >>
  fun p1 <~> p2 = curry fst <$> p1 <*> p2
  fun p1  >> p2 = curry snd <$> p1 <*> p2

  fun one [] = NONE
    | one (y :: ys) = SOME (E.OK y, ys)

  fun eos []       = SOME (E.OK (), [])
    | eos (_ :: _) = NONE

  fun sat p tx xs =
    case tx xs
      of answer as SOME (E.OK y, xs) => if p y then answer else NONE
       | answer => answer

  fun maybe f tx xs =
    case tx xs
      of SOME (E.OK y, xs) =>
           (case f y of SOME z => SOME (E.OK z, xs) | NONE => NONE)
       | SOME (E.ERROR msg, xs) => SOME (E.ERROR msg, xs)
       | NONE => NONE

  fun notFollowedBy t xs =
    case t xs
      of NONE => SOME (E.OK (), xs)
       | SOME _ => NONE

  fun many t = 
    curry (op ::) <$> t <*> (fn xs => many t xs) <|> succeed []

  fun many1 t = 
    curry (op ::) <$> t <*> many t

  fun optional t = 
    SOME <$> t <|> succeed NONE

  fun check p xs =
    Option.map (fn (result, xs) => (Error.join result, xs)) (p xs)

  fun fix mk_p =
    let fun diverge tokens = diverge tokens
        val cell = ref diverge
        val parser = mk_p (fn ts => ! cell ts)
                      (* delay ! until after tokens are delivered,
                         therefore until after cell is updated *)
        val _ = cell := parser
    in  parser
    end

  infix 0 ===

  fun p1 === p2 = (fn tokens => p1 tokens = p2 tokens)


  fun maybe_sat_law f p =
        maybe f p === valOf <$> sat isSome p

  fun demo showToken =
    let
    fun expected what =
      let fun bad t = Error.ERROR ("expected " ^ what ^
                                   ", but instead found this token: " ^ showToken t)
      in check (  bad <$> one
              <|> perror ("expected " ^ what ^ ", but there was no more input")
               )
      end
    in expected
    end

end
