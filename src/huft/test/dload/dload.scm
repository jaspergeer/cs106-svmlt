;; really should implement that list syntax
;;
(define use (filename)
  (let* ([cmd (cons 'huft (cons 'es-vo (cons filename '())))]
         [fd (popen cmd)]
         [module (dload fd)])
         (module)))

;; this should be overwritten when we load loadme.kn
(let ([$r1 'load-failure])
  (set message $r1))

(use 'test/dload/loadme.scm)

(let ([$r1 message])
  (println $r1))

;; (set x (lambda (a) (println a)))

;; (x 2)