;; F.LITERAL, F.CHECK_EXPECT, F.CHECK_ASSERT
;;
(check-expect (number? 3) #t)            
(check-expect (number? 'really?) #f)
(check-assert (symbol? 'really?))

;; global, setglobal
(check-expect (set x 1) 1)
(check-expect x 1)
(set x 45)
(check-expect x 45)
(val y 66)
(check-expect y 66)

;; begin
(check-expect (begin
  (set z 1)
  (set z (number? z))
  (set z (boolean? z)))
  #t)

;; while
(val m 1)
(check-expect
  (while
    (number? m)
    (set m 'done))
    #f)
(check-expect m 'done)

;; if
(check-expect
  (if (number? m) 10 'a) 'a)

;; define
(define fun (x) x)

(check-assert (function? fun))
