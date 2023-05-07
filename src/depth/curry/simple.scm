(use predef.scm)

;; a simple test

(check-expect ((+ 1) 2) 3)

;; return and apply a partially applied function

(define partplus (x) (lambda (z) (+ (+ x z))))

(check-assert (function? ((partplus 6) 89)))
(check-expect (((partplus 6) 89) 4) 99)

;; function returns a partially applied function

(define f (x) (+ x))

(check-expect (f 1 2) 3)
(check-assert (function? (f 1)))

;; use a partially applied function as predicate

(check-expect (filter (>= 5) '(2 4 6 2 41134 2 1 0 9)) '(2 4 2 2 1 0))
