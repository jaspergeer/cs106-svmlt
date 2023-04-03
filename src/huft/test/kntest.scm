;; F.LITERAL, F.CHECK_EXPECT, F.CHECK_ASSERT
;;
(check-expect (number? 3) #t)            
(check-expect (number? 'really?) #f)
(check-assert (symbol? 'really?))

;; ;; global, setglobal
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

;; ;; while
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

;; primcall
(check-expect
  (+
    (*
      (+ 4 5)
      (+ 2 3))
    (/ 10 5))
  47)

;; ;; function call

(check-expect
  (fun 1)
  1)

(define fun2 (x y) (+ x y))

(check-expect
  (fun2 1 2)
  3)

;; y

;; (append (qsort (filter left? rest))
;;         (cons pivot (qsort (filter right? rest))))

;; let expression

;; I got this from cs 105 cqs of the scheme homework

(val x 3)  
(check-expect (let ([x 4] [y x]) y) 
              3)
(check-expect (let* ([x 4] [y x]) y)
              4)
(val y 4)  
(check-expect (let ([x y] [y x]) y)
                3)
(check-expect (let* ([x y] [y x]) y)
                4)
