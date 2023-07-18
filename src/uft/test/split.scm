(define split (xs half other-half)
               (case xs
                  ['() (PAIR half other-half)]
                  [(cons y ys)
                      (split ys other-half (cons y half))]))

(check-expect (split '(1 2) '() '()) (PAIR '(1) '(2)))
(check-expect (split '(1 2 3 4) '() '()) (PAIR '(3 1) '(4 2)))