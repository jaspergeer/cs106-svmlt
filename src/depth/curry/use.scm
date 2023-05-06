;; will be precompiled to use in tests
(define use (filename)
  (let* ([cmd (cons '${BIN_DIR}/uft (cons 'es-vo (cons filename '())))]
         [fd (popen cmd)]
         [module (dload fd)])
         (module)))
