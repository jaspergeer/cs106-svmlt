(define f ()
   (let* ([x 1]
          [z x])
     (begin (set z 2)
            x)))
(check-expect (f) 1)
