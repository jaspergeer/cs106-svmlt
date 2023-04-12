;; a simple test
(check-expect ((+ 1) 2) 3)

;; recursion
(define power (x n)
 (if (= n 1) x (* x (power x (- n 1)))))

(check-expect ((power 2) 4) 16)

;; return a partially applied function
(define partplus (x) (+ x))

(check-expect ((partplus 10) 89) 99)
