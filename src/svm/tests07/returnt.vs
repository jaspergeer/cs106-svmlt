    goto L
    r0 := "This code should never have been executed"
    error r0
    error r0
    error r0
    L:
    r100 := function (0 arguments) {
      r0 := r0
      return r0
    }
    r1 := "test value"
    check "initial assignment to r1", r1
    r0 := call r100
    expect "final value of r1", r1
