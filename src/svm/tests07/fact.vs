    r0 := function (1 arguments) {
      ;; parameter n is in r1
      r2 := 0
      r2 := r1 = r2
      if r2 goto L1
        ;; induction step, n > 0
        r2 := r0  ;; the factorial fucntion
        r3 := 1
        r3 := r1 - r3
        r2 := call r2 (r3)
        r2 := r1 * r2
        return r2
      L1:
        ;; base case: n == 0
        r0 := 1
        return r0
    }
    global factorial := r0

    r1 := 5
    r0 := call r0 (r1)
    check "(factorial 5)", r0
    r0 := 120
    expect "120", r0
