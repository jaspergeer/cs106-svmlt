;; This helped me find a particularly insidious bug

;; Applies multiple arguments to a function which takes a single argument
;; and returns a function.
;; Thus, demonstrates both callstack continuations and partial application.
;; Additionally, uses both call and tailcall to do so.

;; the printed numbers should count up from 0 to 199

(val args-ct 0)

(val n 0)

(define big-nested-fun (n)
  (if (= 0 n) 
    0
    (begin (set args-ct (+ args-ct 1))
    (lambda (x)
      (begin (print x)
             (print '_)
      (big-nested-fun (- n 1)))))))

(let ([f (big-nested-fun 200)])
  (while (function? f)
    (begin (set f (f n (+ n 1) (+ n 2) (+ n 3)))
            (set n (+ n 4)))))

(println 'done)

(check-expect args-ct 200)
