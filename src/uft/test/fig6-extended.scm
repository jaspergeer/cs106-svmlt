(define figure-6 (arg)
  (case arg
    [(C1 C2 C3) 'one]
    [(C1 x C4)  'two]
    [(C1 x C5)  'three]
    [_          'four]))

(check-expect (figure-6 (C1 C2 C3)) 'one)
(check-expect (figure-6 (C1 3 C4)) 'two)
(check-expect (figure-6 (C1 3 C5)) 'three)
(check-expect (figure-6 111) 'four)
