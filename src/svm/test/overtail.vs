;; very similar to tailcallt1
$r0 := fun 1 {
      $r2 := 50000
      $r3 := $r1 = $r2
      if $r3 goto L1
        $r3 := 1
        $r1 := $r1 + $r3
        tailcall $r0 $r1
      def L1
        return $r1
}
G[big] := $r0

$r1 := 0
$r0 := call $r0 $r1
check $r0 "(big 50000)"
$r100 := 50000
expect $r100 "0"
