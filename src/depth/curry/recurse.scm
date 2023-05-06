(use predef.scm)

;; partial application of a recursive function

;; checks that we are placing a closure with no captured arguments in
;; the new reg0

(define power (x n)
  (if (= n 1) x (* x (power x (- n 1)))))

(check-expect ((power 2) 4) 16)

;; use it to map over a list

(check-expect (map (power 2) '(1 2 3 4 5 6)) '(2 4 8 16 32 64))
