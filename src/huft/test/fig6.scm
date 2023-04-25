(define figure-6 (arg)
  (case arg
    [(C1 x) 'one]
    [(C2 x)  'two]
    [(C3 x)  'three]
    [_          'four]
    ))

;; (check-expect (figure-6 (C1 C2 C3)) 'one)
