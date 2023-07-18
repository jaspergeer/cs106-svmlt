
(define nomatch (x)
    (case x
        [C1   1]))

(check-expect (nomatch C2) 1)