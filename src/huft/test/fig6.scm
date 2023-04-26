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

;; (define fig (arg)
;;   (case arg
;;     [C1      'one]
;;     [C2      'two]
;;     [C3      'three]
;;     [_       'two]))

;; (check-expect (fig C1) 'one)


(define figure-6 (arg)
  (case arg
    [(C1 C2 C3) 'one]
    [(C1 x C4)  'two]
    [(C1 x C5)  'three]
    [_          'four]))

(check-expect (figure-6 (C1 3 C4)) 'two)


;; ;; (check-expect (figure-6 (C1 3 C4)) 'three)

;; (define f (x)
;; (case x [C1 'one] [_ 'four])
;; )

;; (check-expect (f C1) 'one)
