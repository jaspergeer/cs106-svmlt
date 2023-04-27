(define map (f xs)
        (case xs
            ['()         '()]
            [(cons x xs)  (cons (f x) xs)])
        )

;; (case xs ['()     '()])

;; ;; (case xs [C     C])