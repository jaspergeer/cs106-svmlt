$r0 := fun 1 {
  $r2 := 0
  $r2 := $r1 = $r2
  if $r2 goto L1
    $r2 := $r0
    $r3 := 1
    $r3 := $r1 - $r3
    $r2 := call $r2 $r3
    $r2 := $r1 * $r2
    return $r2
  def L1
    $r0 := 1
    return $r0
}
G[factorial] := $r0

$r1 := 5
$r0 := call $r0 $r1
check $r0 "(factorial 5)"
$r0 := 120
expect $r0 "120"