(define id (x) x)
(define simplify (x)
  (lambda ()
    (id (lambda () x))))

(check-expect (((simplify 99))) 99)