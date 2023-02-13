halt
$r1 := 0
$r1 := 5
$r1 := $r2 + $r3
check $r1 "check"
expect $r1 "expect"
;; infinite loops
def urmom
goto urmom
if $r1 goto urmom
$r1 := $r2 + $r3
