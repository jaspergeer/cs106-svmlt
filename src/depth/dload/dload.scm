;; simple test, checks that basic dload functionality and literal
;; pool gc are working

;; this will be redefined in loadme.scm
(define callme () 'bad-load)

(use stress.scm)

(use loadme.scm)

(check-expect (callme) 'load-success)

