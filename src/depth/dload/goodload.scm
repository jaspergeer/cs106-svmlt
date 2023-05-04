(define filter (p? xs)
  (if (null? xs)
    '()
    (if (p? (car xs))
      (cons (car xs) (filter p? (cdr xs)))
      (filter p? (cdr xs)))))
;  predefined uScheme functions 129b 
(define map (f xs)
  (if (null? xs)
    '()
    (cons (f (car xs)) (map f (cdr xs)))))
;  predefined uScheme functions 130a 
(define app (f xs)
  (if (null? xs)
    #f
    (begin (f (car xs)) (app f (cdr xs)))))
;  predefined uScheme functions 130b 
(define exists? (p? xs)
  (if (null? xs)
    #f
    (if (p? (car xs)) 
      #t
      (exists? p? (cdr xs)))))
(define all? (p? xs)
  (if (null? xs)
    #t
    (if (p? (car xs))
      (all? p? (cdr xs))
      #f)))
;  predefined uScheme functions 130d 
(define foldr (op zero xs)
  (if (null? xs)
    zero
    (op (car xs) (foldr op zero (cdr xs)))))
(define foldl (op zero xs)
  (if (null? xs)
    zero
    (foldl op (op (car xs) zero) (cdr xs))))