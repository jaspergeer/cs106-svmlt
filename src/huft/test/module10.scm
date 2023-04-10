(define f (x) x)

(define o (f g) (lambda ( x) (f (g x))))

(check-expect 1 1)

(val f (lambda (x y) (cons x y)))

(val flip (lambda (f) (lambda (x) (lambda (y) (f y x)))))

(check-expect (((flip f) 1) 2) (cons 2 1))
