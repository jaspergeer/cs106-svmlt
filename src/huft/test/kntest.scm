;; Each test must be documented by a comment that 
;; names the value constructor or value constructors that it tests.

;; F.LITERAL, F.CHECK_EXPECT, F.CHECK_ASSERT, F.PrimCall
(check-expect (number? 3) #t)
(check-expect (number? 'really?) #f)
(check-assert (symbol? 'really?))
(check-assert (null? '()))
(check-assert (not (null? '(a b c))))
(check-expect (car '(a b c)) 'a)
(check-expect (cdr '(a b c)) '(b c))
(check-expect (cons 'a '(b c)) '(a b c))
(check-expect (and #t #t) #t)
(check-expect (and #t #f) #f)

;; F.Global F.SetGlobal, F.Val, F.CheckExpect, F.Literal, F.GetGlobal
(check-expect (set x 1) 1)
(check-expect x 1)
(set x 45)
(check-expect x 45)
(val y 66)
(check-expect y 66)

;; F.Begin, F.PrimCall, F.SetGlobal, F.Literal, F.CheckExpect, F.Global
(check-expect (begin
  (set z 1)
  (set z (number? z))
  (set z (boolean? z)))
  #t)

;; F.WhileX, F.SetGlobal, F.CheckExpect, F.Literal, F.Global
(val m 1)
(check-expect
  (while
    (number? m)
    (set m 'done))
    #f)
(check-expect m 'done)

;; F.IfX, F.CheckExpect, F.Literal, F.Global
(check-expect
  (if (number? m) 10 'a) 'a)

;; F.Define, F.PrimCall, F.CheckExpect, F.Global, F.Local
(define fun (x) x)
(check-assert (function? fun))

;; F.PrimCall, F.CheckExpect, F.Literal
(check-expect
  (+
    (*
      (+ 4 5)
      (+ 2 3))
    (/ 10 5))
  47)

;; F.FunCall, F.CheckExpect, F.Literal, F.Global
(check-expect
  (fun 1)
  1)

;; F.FunCall, F.CheckExpect, F.Literal, F.Define, F.Global
(define fun2 (x y) (+ x y))
(check-expect
  (fun2 1 2)
  3)

;; F.FunCall, F.CheckExpect, F.Literal, F.Define, F.Global, F.Local
(define fun3 (x y) (cons x y))
(check-expect
  (fun3 1 2)
  (cons 1 2))

;; F.FunCall, F.CheckExpect, F.Literal, F.Define, F.Global
;; commented out this test because it does not run
;; (append (qsort (filter left? rest))
;;         (cons pivot (qsort (filter right? rest))))

;; let expression
;; I got this from cs 105 cqs of the scheme homework

;; F.Let, F.CheckExpect, F.Literal, F.Global, F.Val, F.SetLocal, F.Local
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
