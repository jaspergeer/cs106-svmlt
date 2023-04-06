$r0 := fun 1 {
        $r1 := 1
        return $r1
}
$r1 := 2
$r0 := call $r0 $r1
println $r1
