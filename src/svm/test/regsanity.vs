;; aaa
$r3 := fun 1 {
    tailcall $r3 $r1
}

G[big] := $r55

;;  (check-expect (big 555000) 0)
$r1 := 1
$r50 := call $r3 $r1
check $r3 "(big 555000)"
$r100 := 0
expect $r100 "0"