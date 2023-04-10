$r0 := fun 1 {
	$r3 := closure[$r3,0]
	$r2 := closure[$r2,0]
	copy $r4 $r3
	copy $r5 $r1
	$r4 := call $r4 $r5
	if $r4 goto L0
	$r0 := "even"
	goto L1
	def L0
	$r0 := "odd"
	def L1
	return $r0
}
setglobal $r0 "parity"
getglobal $r0 "parity"
$r1 := 0
$r0 := call $r0 $r1
check $r0 "(parity 0)"
$r0 := "even"
expect $r0 "'even"
