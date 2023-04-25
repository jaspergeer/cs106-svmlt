;; regression tests

;; (define takewhile (p? xs)
;;         (if (null? xs)
;;             '()
;;             (if (p? (car xs))
;;                 (cons (car xs) (takewhile p? (cdr xs)))
;;                 '())))

;;         (define p1? (x) (= x 1))

;;         (check-expect (takewhile p1? '(1 0 1 0 11 0 1)) '(1))

;; (lambda (n)
;;   (letrec ([f 1]) n))


;; (define hi (n)
;;     (let ([f 1]) (f n)))

(define figure-6 (arg)
  (case arg
    [(C1 C2) 'four]))

(check-expect (figure-6 '(1 2)) 1)
