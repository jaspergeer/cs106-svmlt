$r1 := 0
$r1 := 5
;; sadly newline cannot be printed
$r2 := "\n"
print $r1
print $r2
$r1 := $r1 + $r1
check $r1 "check"
expect $r1 "expect"
;; infinite loops
def hi
goto hi
$r1 := truth $r1
if $r1 goto hi
halt
