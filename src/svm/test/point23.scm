; file point23.scm
(define manyclosures (n)
  (let ([x (lambda () 1)])
    (begin
      (while (> n 0)
        (begin (set x (lambda () (let ([y x]) 1)))
               (set n (- n 1))))
      x)
    )
  )

(check-expect (let ([f (manyclosures 10000)])
                (f)) 1)