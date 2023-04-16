; file alloc.scm
(define allocate (N)
  (let ([x #f])
    (begin
      (while (> N 0)
             (begin
               (set x (cons 'a 'b))
               (set N (- N 1))))
      x)))

(check-expect (allocate 1000) (cons 'a 'b))