$r1 := 0
$r1 := 5
;; sadly newline cannot be printed
$r2 := "\n"
$r3 := "\"I contain \t escape \\ characters\"\n"
print $r1
print $r2
$r1 := $r1 + $r1
check $r1 "check"
expect $r1 "expect"
;; infinite loops, does not work with svm
def hi
goto hi
$r1 := truth $r1
if $r1 goto hi
;; type predicates
$r1 := function? $r1
$r1 := pair? $r1
$r1 := symbol? $r1
$r1 := number? $r1
$r1 := not $r1
$r1 := boolean? $r1
$r1 := null? $r1
$r2 := nil? $r2
$r1 := car $r1
$r1 := cdr $r1
$r1 := cons $r1 $r1
;; arithmetic
$r2 := 2
$r3 := 3
$r1 := $r2 / $r3
$r1 := $r2 + $r3
$r1 := $r2 - $r3
$r1 := $r2 * $r3
$r1 := $r2 mod $r3
$r1 := $r2 idiv $r3
;; hash
$r1 := hash $r2
;; load function
$r1 := fun 3 {
    $r2 := 2
    $r3 := 3
    $r4 := $r2 + $r3
}
halt
