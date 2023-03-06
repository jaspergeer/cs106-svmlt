;; aaa
$r255 := fun 2 {
    tailcall $r255 $r1
}

G[big] := $r55

;;  (check-expect (big 555000) 0)
$r1 := 1
$r255 := call $r255 $r1
check $r255 "(big 555000)"
$r100 := 0
expect $r100 "0"