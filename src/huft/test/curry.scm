;; a simple test
(check-expect ((+ 1) 2) 3)

;; recursion
(define power (x n)
(if (= n 1) x (* x (power x (- n 1)))))

(check-expect ((power 2) 4) 16)

;; return and apply a partially applied function
(define partplus (x) (lambda (z) (+ (+ x z))))

(check-expect (((partplus 6) 89) 4) 99)

;; various numbers of arguments

(define manyargs (a b c d e f g h i)
  (+ a (+ b (+ c (+ d (+ e (+ f (+ g (+ h i)))))))))

(check-expect (((manyargs 1 2 3) 4 5 6 7) 8 9) 45)
(check-expect (((manyargs 1) 2 3 4 5 6 7 8) 9) 45)