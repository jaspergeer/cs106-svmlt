;; "good" test for dload, also some use cases for automatic currying

(use goodload.scm)

(check-expect (filter (= 1) '(1 0 1 0 1 )) '(1 1 1))

(check-expect (map (lambda (x) (+ x 1)) '(1 2 3)) '(2 3 4))


