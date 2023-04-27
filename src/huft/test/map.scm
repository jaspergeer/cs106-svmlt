(define map (f xs)
        (case xs
            ['()         '()]
            [(cons x xs)  (cons (f x) xs)])
        )

;; (case xs ['()     '()])

;; ;; (case xs [C     C])

;; (define some (a)
;;     (case a
;;         [C1     1]
;;         [C2     2]))
