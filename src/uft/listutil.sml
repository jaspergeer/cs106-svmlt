structure ListUtil :> sig  
  (* missing from mosml *)
  val mapi : (int * 'a -> 'b) -> 'a list -> 'b list
  val concatMap : ('a -> 'b list) -> ('a list -> 'b list)
  val foldri : (int * 'a * 'b -> 'b) -> 'b -> 'a list -> 'b
end
  =
struct
  fun mapi f xs =
    let fun go k [] = []
          | go k (x::xs) = f (k, x) :: go (k + 1) xs
    in  go 0 xs
    end

  fun concatMap f xs = foldr (fn (x, tail) => f x @ tail) [] xs

  fun foldri f z =
    let fun go k z [] = z
          | go k z (x :: xs) = f (k, x, go (k + 1) z xs)
    in  go 0 z
    end

end


