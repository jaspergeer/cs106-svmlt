(define parity (n)
  (letrec ([odd?  (lambda (m) (if (= m 0) #f (even? (- m 1))))]
           [even? (lambda (m) (if (= m 0) #t (odd? (- m 1))))])
    (if (odd? n) 'odd 'even)))

(check-expect (parity 0) 'even)
(check-expect (parity 1) 'odd)
(check-expect (parity 30) 'even)
(check-expect (parity 91) 'odd)