;; when the bindAnyReg policy is used, y aliases x and the function will
;; always return 99, rather than the value the caller passes.

(define f (v)
  (let* ([x v]
         [y x])
      (begin (set y 99) x)))

(check-expect (f 100) 100)
