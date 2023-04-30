(define map (f xs)
        (case xs
            ['()         '()]
            [(cons x xs)  (cons (f x) xs)])
        )

;; (case xs ['()     '()])

;; ;; (case xs [C     C])

(define theta (c)
    (case c
        [C1     #t]
        [C2     #f]))
