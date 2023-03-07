    ;; (define big (n) (if (= n 0) n (big (- n 1))))
    r0 := function (1 arguments) {
      ; parameter n is in r1
      r2 := 0
      r2 := r1 = r2
      if r2 goto L1
        r2 := 1
        r1 := r1 - r2
        tailcall r0 (r1)
      L1:
        return r1
    }
    global big := r0

    ;;  (check-expect (big 555000) 0)
    r1 := 555000
    r0 := call r0 (r1)
    check "(big 555000)", r0
    r100 := 0
    expect "0", r100
