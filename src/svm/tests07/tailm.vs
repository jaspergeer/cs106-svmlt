;; modified registers
    $r0 := fun 3 {
      ;; function times-plus: $r1 = n, $r2 = m, $r3 = product
      $r4 := 0
      $r4 := $r1 = $r4
      if $r4 goto L1
        $r4 := $r0
        $r5 := 1
        $r5 := $r1 - $r5
        $r6 := $r2
        $r7 := $r2 + $r3
        tailcall $r4 $r7
        def L1
        $return $r3
    }
    global times-plus := $r0

    $r1 := 1200000
    $r2 := 12
    $r3 := 99
    $r0 := call $r0 $r3
    check "(times-plus 1200000 12 99)", $r0
    $r0 := 14400099
    expect "14400099", $r0
