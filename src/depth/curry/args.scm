;; tests using varying numbers of arguments

;; various numbers of arguments

(define manyargs (a b c d e f g h i)
  (+ a (+ b (+ c (+ d (+ e (+ f (+ g (+ h i)))))))))

(check-expect (manyargs 1 2 3 4 5 6 7 8 9) 45)
(check-expect ((manyargs 1 2 3 4 5 6 7 8) 9) 45)
(check-expect (((manyargs 1 2 3) 4 5 6 7) 8 9) 45)
(check-expect (((manyargs 1) 2 3 4 5 6 7 8) 9) 45)

;; tailcalls

(define tailcall (x) (manyargs 1 2 3 4 5 x))

(check-expect ((tailcall 6) 7 8 9) 45)

;; function takes 2 arguments but needs another

(define need1more (x y) (* (+ x y)))

(check-expect (need1more 1 2 3) 9)

;; function takes 1 argument but needs 4 more

(define need4more (x)
  ((lambda (a b c d) (+ a (* b (/ c d)))) x))

(check-expect (need4more 1 2 3 4) (+ 1 (* 2 (/ 3 4))))
