
(define f (pi frontier)
    (case (patternAt pi frontier)
        [Nothing               #t]
        [(Just (P.Var _))      #t]
        [_                     #f])    )

;; [TEST 3
;;     [EDGE-START[ [--("Nothing",0)-->][MATCH,Literal (Bool True)fromList [] END MATCH],
;;                  [--("Just",1)-->][let [TEST 1
;;                     [EDGE-START[[--("P.Var",1)-->][MATCH,Literal (Bool True)fromList [] END MATCH]]
;;                     EDGE-END]
;;                                 [otherwize Just [MATCH,Literal (Bool False)fromList [] END MATCH]]END-TEST] := $r3[1]]]
;;                      EDGE-END]
;;     [otherwize Just [MATCH,Literal (Bool False)fromList [] END MATCH]]
;; END-TEST]

(define f-unested (pi frontier)
    (case (patternAt pi frontier)
        [Nothing             #t]
        [(Just x)             (case x
                                [(P.Var _) #t]
                                [_       #f])]
        [_                   #f]))

;; [TEST 3
;;     [EDGE-START[ [--("Nothing",0)-->][MATCH,Literal (Bool True)fromList [] END MATCH],
;;                  [--("Just",1)-->][let [MATCH,Case (T (Local "x") [((P.Var [_]),Literal (Bool True)),(_,Literal (Bool False))])fromList [("x",1)] END MATCH] := $r3[1]]]
;;      EDGE-END]
;;     [otherwizeJust [MATCH,Literal (Bool False)fromList [] END MATCH]]
;; END-TEST]


;; [TEST 4[EDGE-START[[--("P.Var",1)-->][MATCH,Literal (Bool True)fromList [] END MATCH]]EDGE-END][otherwizeJust [MATCH,Literal (Bool False)fromList [] END MATCH]]END-TEST]

(define g (pi frontier)
    (case (patternAt pi frontier)
        [(Just (P.Apply con pats))  (Just con (length pats))]
        [_                        Nothing]))

(define g-unested (pi frontier)
    (case (patternAt pi frontier)
        [(Just x)               (case x
                                [(P.Apply con pats) (Just con (length pats))]
                                [_                  Nothing])]
        [_                        Nothing]))

