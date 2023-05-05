;; good test for dload, also some use case for automatic currying

;; predef function you need for `use` 

(define use (filename)
  (let* ([cmd (cons 'huft (cons 'es-vo (cons filename '())))]
         [fd (popen cmd)]
         [module (dload fd)])
         (module)))

(use dload/goodload.scm)

(check-expect (filter (= 1) '(1 0 1 0 1 )) '(1 1 1))

(check-expect (map (lambda (x) (+ x 1)) '(1 2 3)) '(2 3 4))


