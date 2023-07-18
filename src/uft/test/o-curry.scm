(define o (f g) (lambda (x) (f (g x))))'

(define curry (f) (lambda (x) (lambda (y) (f x y))))
