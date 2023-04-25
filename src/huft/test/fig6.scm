(define figure-6 (arg)
  (case arg
    [C1 'one]
    [(C1 x C4)  'two]
    [(C1 x C5)  'three]
    [_          'four]))

(check-expect (figure-6 (C1 3 C4)) 'two)