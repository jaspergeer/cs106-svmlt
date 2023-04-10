;; regression tests

(define takewhile (p? xs)
        (if (null? xs)
            '()
            (if (p? (car xs))
                (cons (car xs) (takewhile p? (cdr xs)))
                '())))

        (define p1? (x) (= x 1))

        (check-expect (takewhile p1? '(1 0 1 0 11 0 1)) '(1))

(define hi (n)
  (letrec ([f 1]) n))