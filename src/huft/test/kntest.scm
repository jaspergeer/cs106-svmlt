;; should test every syntactic form of KNF 

;; It is accepted as valid K-normal form by the UFT checker uft kn-kn.

;; Every syntactic form of K-normal form occurs in a
;; position that effects the result of primitives check and expect.

;;  literal, VMOP
(let* ([$r0 2] [$r1 2] [$r0 (+ $r0 $r1)])
  (check $r0 'two-plus-two))
(let* ([$r0 4])
  (expect $r0 'four))

;; variable, can pass but not together
(let* ([$r0 2] [$r1 $r0])
    (check $r1 'two))
(let* ([$r1 2])
    (expect $r1 'two))

;; If expression, still wierd that I cannot make two tests
;; run at the same time
(let* ([$r0 1] [$r4 (truth $r0)] [$r1 2] [$r2 3] [$r0 (if $r4 $r1 $r2)])
    (check $r0 'two))
(let* ([$r3 2])
    (expect $r3 'two))
