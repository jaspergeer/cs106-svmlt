$r0 := fun 1 {
	$r0 := 1
	return $r0
}
G[f] := $r0
$r0 := G[f]
println $r0
$r1 := 2
$r0 := call $r0 $r1
println $r0
