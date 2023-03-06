    r200 := function (1 arguments) {
      check "argument", r1
      r100 := "parameter test"
      expect "internal literal", r100
      halt
    }
    r201 := "parameter test"
    r0 := call r200 (r201)
