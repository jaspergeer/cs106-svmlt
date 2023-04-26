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


;; (define figure-6 (arg)
;;   (case arg
;;     [(C1 C2 C3) 'one]
;;     [(C1 x C4)  'two]
;;     [(C1 x C5)  'three]
;;     [_          'four]))

;; (check-expect (figure-6 (C1 3 C4)) 'two)


;; (check-expect (figure-6 (C1 3 C4)) 'three)

(define f (x)
(case x 
  [(C1 C2) 'one] 
  [_ 'four]))

(check-expect (f (C1 C2)) 'one)


;; (let ([r0 (lambda (r1) 
;;              (cond   
;;                 [(matches-vcon-arity? r1 'C1 0) 'one] 
;;                 [#t 'four]))]) 
;;   (set f r0))
;; (begin 
;;    (let ([r0 (let* ([r0 f]
;;                     [r1 'C1]) 
;;                (r0 r1))]) 
;;      (check r0 '(f 'C1))) 
;;    (let ([r0 'one]) 
;;      (expect r0 ''one)))

;; (let ([$r0 (lambda ($r1)
;;          (cond   [(matches-vcon-arity? $r1 'C1 0) 'one] 
;;                  [#t 'four]))])
;;    (set f $r0))

;; (begin (let ([$r0 (let ([$r0 f])
;;                 (let ([$r1 (set $r1 (let ([$r2 'C1]) (block $r2)))])
;;                    ($r0 $r1)))])
;;       (check $r0 '(f 'C1)))
;;    (let ([$r0 'one])
;;       (expect $r0 ''one)))


;; (let ([r0 (lambda (r1) 
;;              (cond   
;;                 [(matches-vcon-arity? r1 'C1 1) 
;;                       (let ([r2 (getblockslot r1 1)]) 
;;                         (cond   
;;                            [(matches-vcon-arity? r2 'C2 0) 'one] 
;;                            [#t 'four]))] 
;;                 [#t 'four]))]) 
;;   (set f r0))
;; (begin 
;;    (let ([r0 (let* ([r0 f]
;;                     [r1 (set r1 
;;                            (let* ([r2 'C1]
;;                                   [r3 'C2]) 
;;                              (block r2 r3)))]) 
;;                (r0 r1))]) 
;;      (check r0 '(f ('C1 'C2)))) 
;;    (let ([r0 'one]) 
;;      (expect r0 ''one)))

;; (let ([$r0 (lambda ($r1)
;;          (cond  
;;             [(matches-vcon-arity? $r1 'C1 1)
;;                   (let ([$r2 (getblkslot $r1 1)])
;;                      (cond  
;;                         [(matches-vcon-arity? $r2 'C2 0) 'one]
;;                         [#t 'four]))]
;;             [#t 'four]))])
;;    (set f $r0))
;; (begin (let ([$r0 (let ([$r0 f])
;;                 (let ([$r1 
;;                          (let ([$r1 'C1])
;;                          (let ([$r2 'C2])
;;                             ($r1 $r2)))])
;;                    ($r0 $r1)))])
;;       (check $r0 '(f ('C1 'C2))))
;;    (let ([$r0 'one])
;;       (expect $r0 ''one)))