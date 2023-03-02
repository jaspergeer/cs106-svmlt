;; should test every syntactic form of KNF 

;; It is accepted as valid K-normal form by the UFT checker uft kn-kn.

;; Every syntactic form of K-normal form occurs in a
;; position that effects the result of primitives check and expect.

;;  literal, VMOP
(let* ([$r0 2] [$r1 2] [$r0 (+ $r0 $r1)])
  (check $r0 'two-plus-two))
(let* ([$r0 4])
  (expect $r0 'four))

;; variable
(let* ([$r0 2] [$r1 $r0])
    (check $r1 'two))
(let* ([$r1 2])
    (expect $r1 'two))

;; If expression
(let* ([$r0 1] [$r4 #t] [$r1 2] [$r2 3] [$r0 (if $r4 $r1 $r2)])
    (check $r0 'two))
(let* ([$r3 2])
    (expect $r3 'two))

(let* ([$r0 1] [$r4 (truth $r0)] [$r1 2] [$r2 3] [$r0 (if $r4 $r1 $r2)])
    (check $r0 'two))
(let* ([$r3 2])
    (expect $r3 'two))

;; let already appeared too many times

;; Seq, should evaluate to the value of the last exp
(let* ([$r0 1] [$r1 2] [$r0 (begin $r0 $r1)])
    (check $r0 'two))
(let* ([$r1 2])
    (expect $r1 'two))

;; Assign
(let* ([$r0 1] [$r1 2] [$r2 (set $r0 $r1)])
    (check $r0 'two))
(let* ([$r0 2])
    (expect $r0 'two))

;; While
;; function get_literal, file iparsers.c, line 161.
(let* ([$r0 3]
       [$r3 1]
       [$r4 0])
      (while (let ([$r2 ( > $r0 $r4)])
                    $r2)
              (set $r0 (- $r0 $r3))))
(let ([$r0 1])
  (while (let ([$r1 #t]) $r1) $r0))

(let ([$r0 1])
  (while (let ([$r1 #t]) $r1) $r0))

;; begin
(let ([$r0 1])
  (begin
    (check $r0 'one)
    (expect $r0 'one)))

(let ([$r0 1])
  (begin
    (check $r0 'one)
    (expect $r0 'one)))

;; VMOPGLO

(let ([$r1 1])
    (begin (set hi $r1) 
      (let ([$r2 hi])
        (begin
          (check $r2 'one)
          (expect $r2 'one)))))


(let* ([$r1 1] [$r2 (set $r5 $r1)]) $r1)
