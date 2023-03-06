    r0 := function (0 arguments) {
      r1 := "arrived"
      check "arrived", r1
      expect "arrived", r1
      halt
    }
    r0 := call r0 ()
