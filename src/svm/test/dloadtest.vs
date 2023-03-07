$r1 := popen "./bin/huft vs-vo src/svm/test/loadme.vs"
$r2 := dload $r1
$r0 := call $r2 $r2
$r3 := G[message]
println $r3