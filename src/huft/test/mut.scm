(letrec (
    [x (lambda () 1)]
    [y (lambda () (set x 1))])
    (x)
)
