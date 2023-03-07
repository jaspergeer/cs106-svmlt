    r100 := function (0 arguments) {
      r1 := 1852
      return r1
    }
    r50 := call r100
    check "result from calling function", r50
    r200 := 1852
    expect "value we expected", r200
