(define use (filename)
  (let* ([cmd (cons './bin/uft (cons 'es-vo (cons filename '())))]
         [fd (popen cmd)]
         [module (dload fd)])
         (module)))

;; (define callme ()
;;   (println 'bad-load))

;; (use 'src/depth/dload/loadme.scm)
(use 'src/depth/dload/stress.scm)

;; (println (filter (= 1) '(1 2 3 4 5)))

(val x77 'v77)
(val x78 'v78)
(val x79 'v79)
(val x770 'v737)
(val x7-1 'v718)
(val x739 'v739)
