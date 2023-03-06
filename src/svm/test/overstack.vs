;; aaa
$r0 := fun 1 {
  $r2 := 0
  $r2 := $r1 = $r2
  if $r2 goto L1
    $r2 := 1
    $r1 := $r1 - $r2
    $r0 := call $r0 $r1
  def L1
    return $r1
}
G[big] := $r0

;;  (check-expect (big 555000) 0)
$r1 := 555000
$r0 := call $r0 $r1
check $r0 "(big 555000)"
$r100 := 0
expect $r100 "0"