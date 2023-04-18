$r0 := fun 1 {
	$r2 := #f
	goto L0
	def L1
	$r3 := "a"
	$r4 := "b"
	$r2 := cons $r3 $r4
	$r3 := 1
	$r1 := $r1 - $r3
	def L0
	$r3 := 0
	$r3 := $r1 > $r3
	if $r3 goto L1
	return $r2
}
setglobal $r0 "allocate"
getglobal $r0 "allocate"
$r1 := 1000
$r0 := call $r0 $r1
check $r0 "(allocate 1000)"
$r0 := "a"
$r1 := "b"
$r0 := cons $r0 $r1
expect $r0 "(cons 'a 'b)"
