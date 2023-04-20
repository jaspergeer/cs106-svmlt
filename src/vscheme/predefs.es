(define null? (xs)
   (case xs
      [(cons y ys) #f]
      ['()         #t]))
(check-principal-type* null? (forall ['a] ((list 'a) -> bool)))
(data (* * => *) pair
  [PAIR : (forall ['a 'b] ('a 'b -> (pair 'a 'b)))])
(define pair (x y) (PAIR x y))
(define fst (p)
   (case p [(PAIR x _) x]))
(define snd (p)
   (case p [(PAIR _ y) y]))
(define Int.compare (n1 n2)
  (if (< n1 n2) LESS
      (if (< n2 n1) GREATER
          EQUAL)))
(check-principal-type* Int.compare (int int -> order))
(define null? (xs)
   (case xs ['()        #t]
            [(cons _ _) #f]))
(define car (xs)
   (case xs ['()        (error 'car-of-empty-list)]
            [(cons y _) y]))
(define cdr (xs)
   (case xs ['()         (error 'cdr-of-empty-list)]
            [(cons _ ys) ys]))
(check-principal-type* null? (forall ['a] ((list 'a) -> bool)))
(check-principal-type* car (forall ['a] ((list 'a) -> 'a)))
(check-principal-type* cdr (forall ['a] ((list 'a) -> (list 'a))))
(define append (xs ys)
  (case xs
     ['()         ys]
     [(cons z zs) (cons z (append zs ys))]))

(define revapp (xs ys)
  (case xs
     ['()         ys]
     [(cons z zs) (revapp zs (cons z ys))]))
(define list1 (x) (cons x '()))
(define bind (x y alist)
  (case alist
     ['() (list1 (pair x y))]
     [(cons p ps)
        (if (= x (fst p))
            (cons (pair x y) ps)
            (cons p (bind x y ps)))]))
(check-principal-type* append (forall ['a] ((list 'a) (list 'a) -> (list 'a))))
(check-principal-type* revapp (forall ['a] ((list 'a) (list 'a) -> (list 'a))))
(check-principal-type* bind (forall ['a 'b] ('a 'b (list (pair 'a 'b)) -> (list (pair 'a 'b)))))
(define find (x alist)
  (case alist
       ['()   NONE]
       [(cons (PAIR key value) pairs)
          (if (= x key)
              (SOME value)
              (find x pairs))]))
(check-principal-type* find (forall ['a 'b] ('a (list (pair 'a 'b)) -> (option 'b))))
(define bound? (x alist)
  (case (find x alist)
     [(SOME _) #t]
     [NONE     #f]))
(check-principal-type* bound? (forall ['a 'b] ('a (list (pair 'a 'b)) -> bool)))
(define and (b c) (if b  c  b))
(define or  (b c) (if b  b  c))
(define not (b)   (if b #f #t))
(define o (f g) (lambda (x) (f (g x))))
(define curry   (f) (lambda (x) (lambda (y) (f x y))))
(define uncurry (f) (lambda (x y) ((f x) y)))
(define caar (xs) (car (car xs)))
(define cadr (xs) (car (cdr xs)))
(define cdar (xs) (cdr (car xs)))
(define filter (p? xs)
  (case xs
     ['()   '()]
     [(cons y ys)  (if (p? y) (cons y (filter p? ys))
                              (filter p? ys))]))
(define map (f xs)
  (case xs
     ['() '()]
     [(cons y ys) (cons (f y) (map f ys))]))
(define app (f xs)
  (case xs
     ['() UNIT]
     [(cons y ys) (begin (f y) (app f ys))]))
(define reverse (xs) (revapp xs '()))
(define exists? (p? xs)
  (case xs
     ['() #f]
     [(cons y ys) (if (p? y) #t (exists? p? ys))]))
(define all? (p? xs)
  (case xs
     ['() #t]
     [(cons y ys) (if (p? y) (all? p? ys) #f)]))
(define foldr (op zero xs)
  (case xs
     ['() zero]
     [(cons y ys) (op y (foldr op zero ys))]))
(define foldl (op zero xs)
  (case xs
     ['() zero]
     [(cons y ys) (foldl op (op y zero) ys)]))
(define <= (x y) (not (> x y)))
(define >= (x y) (not (< x y)))
(define != (x y) (not (= x y)))
(define max (m n) (if (> m n) m n))
(define min (m n) (if (< m n) m n))
(define negated (n) (- 0 n))
(define mod (m n) (- m (* n (/ m n))))
(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))
(define lcm (m n) (* m (/ n (gcd m n))))
(define min* (xs) (foldr min (car xs) (cdr xs)))
(define max* (xs) (foldr max (car xs) (cdr xs)))
(define gcd* (xs) (foldr gcd (car xs) (cdr xs)))
(define lcm* (xs) (foldr lcm (car xs) (cdr xs)))
(define list1 (x)               (cons x '()))
(define list2 (x y)             (cons x (list1 y)))
(define list3 (x y z)           (cons x (list2 y z)))
(define list4 (x y z a)         (cons x (list3 y z a)))
(define list5 (x y z a b)       (cons x (list4 y z a b)))
(define list6 (x y z a b c)     (cons x (list5 y z a b c)))
(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))
(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))
(define takewhile (p? xs)
  (case xs
     ['() '()]
     [(cons y ys)
        (if (p? y)
            (cons y (takewhile p? ys))
            '())]))
(define dropwhile (p? xs)
  (case xs
     ['() '()]
     [(cons y ys)
        (if (p? y)
            (dropwhile p? ys)
            xs)]))
