;; a simple test

(check-expect ((+ 1) 2) 3)

;; recursion

(define power (x n)
(if (= n 1) x (* x (power x (- n 1)))))

(check-expect ((power 2) 4) 16)

;; return and apply a partially applied function

(define partplus (x) (lambda (z) (+ (+ x z))))

(check-assert (function? ((partplus 6) 89)))
(check-expect (((partplus 6) 89) 4) 99)

;; various numbers of arguments

(define manyargs (a b c d e f g h i)
  (+ a (+ b (+ c (+ d (+ e (+ f (+ g (+ h i)))))))))

(check-expect (manyargs 1 2 3 4 5 6 7 8 9) 45)
(check-expect ((manyargs 1 2 3 4 5 6 7 8) 9) 45)
(check-expect (((manyargs 1 2 3) 4 5 6 7) 8 9) 45)
(check-expect (((manyargs 1) 2 3 4 5 6 7 8) 9) 45)

;; tailcalls

(define tailcall (x) (manyargs 1 2 3 4 5 x))

(check-expect ((tailcall 6) 7 8 9) 45)

;; extra args

(define f (x) (+ x))

(check-expect (f 1 2) 3)
(check-assert (function? (f 1)))

(define needmore1 (x y) (* (+ x y)))

(check-expect (needmore1 1 2 3) 9)

(define needmore2 (x)
  ((lambda (a b c d) (+ a (* b (/ c d)))) x))

(check-expect (needmore2 1 2 3 4) (+ 1 (* 2 (/ 3 4))))
