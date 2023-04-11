(letrec (
  [x (lambda () 1)]
  [y (lambda () (begin (set x 1) 5))])
  (check-expect (x) 1))