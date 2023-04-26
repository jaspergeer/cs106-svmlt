;; (define figure-6 (arg)
;;   (case arg
;;     [(C1 C2) 'one]
;;     [(C1 x C4)  'two]
;;     [(C1 x C5)  'three]
;;     [_          'four]))

;; (define figure-6 (arg)
;;   (case arg
;;     [(C1 C2 C3)  'one]
;;     [(C1 C2 C4)  'two]
;;     [(C1 x  y) 'three]
;;     [_          'four]))

(define fig (arg)
  (case arg
    [C1      'one]
    [_       'two]))

(check-expect (fig C1) 'one)

;; (check-expect (figure-6 (C1 3 C4)) 'three)


;; (case x [C1 3 C4])
;; (figure-6 (C1 3 C4))