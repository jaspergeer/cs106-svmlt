;; (val flip 
;;    (mkclosure 
;;       (lambda ($closure f) 
;;          (mkclosure 
;;             (lambda ($closure x y) ((CAPTURED-IN 0 $closure) y x)) 
;;             (cons f '())))
;;       '()))

(val o
    (mkclosure
        (lambda ($closure f g)
            (mkclosure
                (lambda ($closure x) ((CAPTURED-IN 0 $closure) ((CAPTURED-IN 1 $closure) x)))
                (cons f (cons g '()))))
    '()))

(begin (let ([$r0 (let ([$r0 (let ([$r0 o])
                           (let ([$r1 (lambda ($r1) (not $r0))])
                              (let ([$r2 (lambda ($r1) (not $r0))])
                                 ($r0 $r1 $r2))))])
                (let ([$r1 #t])
                   ($r0 $r1)))])
      (check $r0 '((o not not) #t)))
   (let ([$r0 #t])
      (expect $r0 '#t)))

;; (define qsort (xs)
;;   (if (null? xs)
;;       '()
;;       (let* ([pivot  (car xs)]
;;              [rest   (cdr xs)]
;;              [right? (lambda (n) (> n pivot))]
;;              [left?  (o not right?)])
;;         (append (qsort (filter left? rest))
;;                 (cons pivot (qsort (filter right? rest)))))))

(val qsort 
    (mkclosure
        (lambda ($closure xs)
            (if (null? xs)
                '()
                (let* ([pivot  (car xs)]
                       [rest   (cdr xs)]
                       [right? (mkclosure (lambda ($closure n) (> n (CAPTURED-IN 0 $closure))) (cons pivot '()))]
                       [left?  (o not right?)])
                    (append (qsort (filter left? rest))
                            (cons pivot (qsort (filter right? rest)))))))
        '()
))

(define iota^ (n)
  ; return reversed list of natural numbers 1..n
  (if (= n 0) '() (cons n (iota^ (- n 1)))))

(check-expect 
  (qsort '(65 15 87 42 62 45 6 81 53 34 33 82 79 7 17 39 71 18 98 92 77 41 51 16 86 30 49 10 4 68 35 52 69 12 85 36 47 5 1 61 74 64 31 80 25 29 93 78 72 24 99 48 76 19 66 70 3 56 23 32 84 100 91 58 20 60 26 37 97 54 46 13 21 63 28 14 59 67 38 88 57 40 55 94 11 95 22 44 27 9 83 50 43 8 90 73 75 96 89 2 ))
  (reverse (iota^ 100)))


