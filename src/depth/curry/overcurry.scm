(val args-ct 0)

(define big-nested-fun (n)
  (if (= 0 n) 
    0
    (begin (set args-ct (+ args-ct 1))
    (lambda (x)
      (begin (print x)
      (big-nested-fun (- n 1)))))))

(let ([f (big-nested-fun 200)])
  (while (function? f)
    (set f (f 1 2 3 4))))

(check-expect args-ct 200)
