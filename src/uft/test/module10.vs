$r0 := fun 1 {
	return $r1
}
setglobal $r0 "f"

$r0 := fun 2 {
	$r0 := fun 1 {
		$r2 := $r0.0
		$r3 := $r0.1
		copy $r4 $r1
		$r3 := call $r3 $r4
		tailcall $r2 $r3
	}
	$r0 := closure[$r0,2]
	$r0.0 := $r1
	$r0.1 := $r2
	return $r0
}

setglobal $r0 "o"

getglobal $r0 "o"

$r1 := fun 1 {
	$r0 := not $r1
	return $r0
}

$r2 := fun 1 {
	$r0 := not $r1
	return $r0
}

$r0 := call $r0 $r2

$r1 := #f

$r0 := call $r0 $r1

check $r0 "((o not not) #f)"

$r0 := #f

expect $r0 "#f"
