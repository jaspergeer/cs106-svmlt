; file grow.scm
(define iota (N)
  (let ([ns '()])
    (begin
      (while (> N 0)
             (begin
               (set ns (cons N ns))
               (set N (- N 1))))
      ns)))

(check-expect (iota 8) '(1 2 3 4 5 6 7 8))

(define allocate (N)
  (let ([x #f])
    (begin
      (while (> N 0)
             (begin
               (set x (iota N))
               (set N (- N 1))))
      x)))

(check-expect (allocate 1000) '(1))