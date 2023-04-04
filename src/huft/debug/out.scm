$r0 := fun 1 {
	$r2 := car $r1
	$r0 := car $r2
	return $r0
}
setglobal $r0 "caar"
$r0 := fun 1 {
	$r2 := cdr $r1
	$r0 := car $r2
	return $r0
}
setglobal $r0 "cadr"
$r0 := fun 1 {
	$r2 := car $r1
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cdar"
$r0 := fun 1 {
	$r2 := cdr $r1
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cddr"
$r0 := fun 1 {
	getglobal $r2 "caar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "caaar"
$r0 := fun 1 {
	getglobal $r2 "cadr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "caadr"
$r0 := fun 1 {
	getglobal $r2 "cdar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "cadar"
$r0 := fun 1 {
	getglobal $r2 "cddr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "caddr"
$r0 := fun 1 {
	getglobal $r2 "caar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cdaar"
$r0 := fun 1 {
	getglobal $r2 "cadr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cdadr"
$r0 := fun 1 {
	getglobal $r2 "cdar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cddar"
$r0 := fun 1 {
	getglobal $r2 "cddr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cdddr"
$r0 := fun 1 {
	getglobal $r2 "caaar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "caaaar"
$r0 := fun 1 {
	getglobal $r2 "caadr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "caaadr"
$r0 := fun 1 {
	getglobal $r2 "cadar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "caadar"
$r0 := fun 1 {
	getglobal $r2 "caddr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "caaddr"
$r0 := fun 1 {
	getglobal $r2 "cdaar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "cadaar"
$r0 := fun 1 {
	getglobal $r2 "cdadr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "cadadr"
$r0 := fun 1 {
	getglobal $r2 "cddar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "caddar"
$r0 := fun 1 {
	getglobal $r2 "cdddr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := car $r2
	return $r0
}
setglobal $r0 "cadddr"
$r0 := fun 1 {
	getglobal $r2 "caaar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cdaaar"
$r0 := fun 1 {
	getglobal $r2 "caadr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cdaadr"
$r0 := fun 1 {
	getglobal $r2 "cadar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cdadar"
$r0 := fun 1 {
	getglobal $r2 "caddr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cdaddr"
$r0 := fun 1 {
	getglobal $r2 "cdaar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cddaar"
$r0 := fun 1 {
	getglobal $r2 "cdadr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cddadr"
$r0 := fun 1 {
	getglobal $r2 "cddar"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cdddar"
$r0 := fun 1 {
	getglobal $r2 "cdddr"
	copy $r3 $r1
	$r2 := call $r2 $r3
	$r0 := cdr $r2
	return $r0
}
setglobal $r0 "cddddr"
$r0 := fun 1 {
	$r2 := '()
	$r0 := cons $r1 $r2
	return $r0
}
setglobal $r0 "list1"
$r0 := fun 2 {
	getglobal $r3 "list1"
	copy $r4 $r2
	$r3 := call $r3 $r4
	$r0 := cons $r1 $r3
	return $r0
}
setglobal $r0 "list2"
$r0 := fun 3 {
	getglobal $r4 "list2"
	copy $r5 $r2
	copy $r6 $r3
	$r4 := call $r4 $r6
	$r0 := cons $r1 $r4
	return $r0
}
setglobal $r0 "list3"
$r0 := fun 2 {
	$r3 := null? $r1
	if $r3 goto L0
	$r3 := car $r1
	getglobal $r4 "append"
	$r5 := cdr $r1
	copy $r6 $r2
	$r4 := call $r4 $r6
	$r0 := cons $r3 $r4
	return $r0
	def L0
	return $r2
}
setglobal $r0 "append"
$r0 := fun 2 {
	$r3 := null? $r1
	if $r3 goto L1
	getglobal $r3 "revapp"
	$r4 := cdr $r1
	$r5 := car $r1
	$r5 := cons $r5 $r2
	tailcall $r3 $r5
	def L1
	return $r2
}
setglobal $r0 "revapp"
$r0 := fun 1 {
	getglobal $r2 "revapp"
	copy $r3 $r1
	$r4 := '()
	tailcall $r2 $r4
}
setglobal $r0 "reverse"
$r0 := fun 2 {
	$r3 := 0
	$r3 := $r1 = $r3
	if $r3 goto L2
	getglobal $r3 "nth"
	$r4 := 1
	$r4 := $r1 - $r4
	$r5 := cdr $r2
	tailcall $r3 $r5
	def L2
	$r0 := car $r2
	return $r0
}
setglobal $r0 "nth"
$r0 := fun 2 {
	if $r1 goto L3
	return $r1
	def L3
	return $r2
}
setglobal $r0 "and"
$r0 := fun 1 {
	$r2 := symbol? $r1
	$r3 := number? $r1
	$r4 := boolean? $r1
	$r5 := null? $r1
	$r4 := $r4 or $r5
	$r3 := $r3 or $r4
	$r0 := $r2 or $r3
	return $r0
}
setglobal $r0 "atom?"
$r0 := fun 2 {
	getglobal $r3 "atom?"
	copy $r4 $r1
	$r3 := call $r3 $r4
	if $r3 goto L4
	getglobal $r3 "atom?"
	copy $r4 $r2
	$r3 := call $r3 $r4
	if $r3 goto L5
	getglobal $r3 "equal?"
	$r4 := car $r1
	$r5 := car $r2
	$r3 := call $r3 $r5
	getglobal $r4 "equal?"
	$r5 := cdr $r1
	$r6 := cdr $r2
	$r4 := call $r4 $r6
	$r0 := $r3 and $r4
	return $r0
	def L5
	$r0 := #f
	return $r0
	def L4
	$r0 := $r1 = $r2
	return $r0
}
setglobal $r0 "equal?"
$r0 := fun 2 {
	getglobal $r3 "list2"
	copy $r4 $r1
	copy $r5 $r2
	tailcall $r3 $r5
}
setglobal $r0 "make-alist-pair"
$r0 := fun 1 {
	$r0 := car $r1
	return $r0
}
setglobal $r0 "alist-pair-key"
$r0 := fun 1 {
	getglobal $r2 "cadr"
	copy $r3 $r1
	tailcall $r2 $r3
}
setglobal $r0 "alist-pair-attribute"
$r0 := fun 1 {
	getglobal $r2 "alist-pair-key"
	$r3 := car $r1
	tailcall $r2 $r3
}
setglobal $r0 "alist-first-key"
$r0 := fun 1 {
	getglobal $r2 "alist-pair-attribute"
	$r3 := car $r1
	tailcall $r2 $r3
}
setglobal $r0 "alist-first-attribute"
$r0 := fun 3 {
	$r4 := null? $r3
	if $r4 goto L6
	getglobal $r4 "equal?"
	copy $r5 $r1
	getglobal $r6 "alist-first-key"
	copy $r7 $r3
	$r6 := call $r6 $r7
	$r4 := call $r4 $r6
	if $r4 goto L7
	$r4 := car $r3
	getglobal $r5 "bind"
	copy $r6 $r1
	copy $r7 $r2
	$r8 := cdr $r3
	$r5 := call $r5 $r8
	$r0 := cons $r4 $r5
	return $r0
	def L7
	getglobal $r4 "make-alist-pair"
	copy $r5 $r1
	copy $r6 $r2
	$r4 := call $r4 $r6
	$r5 := cdr $r3
	$r0 := cons $r4 $r5
	return $r0
	def L6
	getglobal $r4 "list1"
	getglobal $r5 "make-alist-pair"
	copy $r6 $r1
	copy $r7 $r2
	$r5 := call $r5 $r7
	tailcall $r4 $r5
}
setglobal $r0 "bind"
$r0 := fun 2 {
	$r3 := null? $r2
	if $r3 goto L8
	getglobal $r3 "equal?"
	copy $r4 $r1
	getglobal $r5 "alist-first-key"
	copy $r6 $r2
	$r5 := call $r5 $r6
	$r3 := call $r3 $r5
	if $r3 goto L9
	getglobal $r3 "find"
	copy $r4 $r1
	$r5 := cdr $r2
	tailcall $r3 $r5
	def L9
	getglobal $r3 "alist-first-attribute"
	copy $r4 $r2
	tailcall $r3 $r4
	def L8
	$r0 := '()
	return $r0
}
setglobal $r0 "find"
$r0 := fun 2 {
	$r3 := null? $r2
	if $r3 goto L10
	copy $r3 $r1
	$r4 := car $r2
	$r3 := call $r3 $r4
	if $r3 goto L11
	getglobal $r3 "filter"
	copy $r4 $r1
	$r5 := cdr $r2
	tailcall $r3 $r5
	def L11
	$r3 := car $r2
	getglobal $r4 "filter"
	copy $r5 $r1
	$r6 := cdr $r2
	$r4 := call $r4 $r6
	$r0 := cons $r3 $r4
	return $r0
	def L10
	$r0 := '()
	return $r0
}
setglobal $r0 "filter"
$r0 := fun 2 {
	$r3 := null? $r2
	if $r3 goto L12
	copy $r3 $r1
	$r4 := car $r2
	$r3 := call $r3 $r4
	getglobal $r4 "map"
	copy $r5 $r1
	$r6 := cdr $r2
	$r4 := call $r4 $r6
	$r0 := cons $r3 $r4
	return $r0
	def L12
	$r0 := '()
	return $r0
}
setglobal $r0 "map"
$r0 := fun 2 {
	$r3 := null? $r2
	if $r3 goto L13
	copy $r3 $r1
	$r4 := car $r2
	$r3 := call $r3 $r4
	getglobal $r3 "app"
	copy $r4 $r1
	$r5 := cdr $r2
	tailcall $r3 $r5
	def L13
	$r0 := #f
	return $r0
}
setglobal $r0 "app"
$r0 := fun 2 {
	$r3 := null? $r2
	if $r3 goto L14
	copy $r3 $r1
	$r4 := car $r2
	$r3 := call $r3 $r4
	if $r3 goto L15
	getglobal $r3 "exists?"
	copy $r4 $r1
	$r5 := cdr $r2
	tailcall $r3 $r5
	def L15
	$r0 := #t
	return $r0
	def L14
	$r0 := #f
	return $r0
}
setglobal $r0 "exists?"
$r0 := fun 2 {
	$r3 := null? $r2
	if $r3 goto L16
	copy $r3 $r1
	$r4 := car $r2
	$r3 := call $r3 $r4
	if $r3 goto L17
	$r0 := #f
	return $r0
	def L17
	getglobal $r3 "all?"
	copy $r4 $r1
	$r5 := cdr $r2
	tailcall $r3 $r5
	def L16
	$r0 := #t
	return $r0
}
setglobal $r0 "all?"
$r0 := fun 3 {
	$r4 := null? $r3
	if $r4 goto L18
	copy $r4 $r1
	$r5 := car $r3
	getglobal $r6 "foldr"
	copy $r7 $r1
	copy $r8 $r2
	$r9 := cdr $r3
	$r6 := call $r6 $r9
	tailcall $r4 $r6
	def L18
	return $r2
}
setglobal $r0 "foldr"
$r0 := fun 3 {
	$r4 := null? $r3
	if $r4 goto L19
	getglobal $r4 "foldl"
	copy $r5 $r1
	copy $r6 $r1
	$r7 := car $r3
	copy $r8 $r2
	$r6 := call $r6 $r8
	$r7 := cdr $r3
	tailcall $r4 $r7
	def L19
	return $r2
}
setglobal $r0 "foldl"
$r0 := 10
setglobal $r0 "newline"
$r0 := 40
setglobal $r0 "left-round"
$r0 := 32
setglobal $r0 "space"
$r0 := 41
setglobal $r0 "right-round"
$r0 := 59
setglobal $r0 "semicolon"
$r0 := 123
setglobal $r0 "left-curly"
$r0 := 39
setglobal $r0 "quotemark"
$r0 := 125
setglobal $r0 "right-curly"
$r0 := 91
setglobal $r0 "left-square"
$r0 := 93
setglobal $r0 "right-square"
$r0 := fun 2 {
	$r3 := $r1 > $r2
	$r0 := not $r3
	return $r0
}
setglobal $r0 "<="
$r0 := fun 2 {
	$r3 := $r1 < $r2
	$r0 := not $r3
	return $r0
}
setglobal $r0 ">="
$r0 := fun 2 {
	$r3 := $r1 = $r2
	$r0 := not $r3
	return $r0
}
setglobal $r0 "!="
$r0 := fun 2 {
	$r3 := $r1 > $r2
	if $r3 goto L20
	return $r2
	def L20
	return $r1
}
setglobal $r0 "max"
$r0 := fun 2 {
	$r3 := $r1 < $r2
	if $r3 goto L21
	return $r2
	def L21
	return $r1
}
setglobal $r0 "min"
$r0 := fun 1 {
	$r2 := 0
	$r0 := $r2 - $r1
	return $r0
}
setglobal $r0 "negated"
$r0 := fun 2 {
	$r3 := $r1 idiv $r2
	$r3 := $r2 * $r3
	$r0 := $r1 - $r3
	return $r0
}
setglobal $r0 "mod"
$r0 := fun 2 {
	$r3 := 0
	$r3 := $r2 = $r3
	if $r3 goto L22
	getglobal $r3 "gcd"
	copy $r4 $r2
	$r5 := $r1 mod $r2
	tailcall $r3 $r5
	def L22
	return $r1
}
setglobal $r0 "gcd"
$r0 := fun 2 {
	$r3 := 0
	$r3 := $r1 = $r3
	if $r3 goto L23
	getglobal $r3 "gcd"
	copy $r4 $r1
	copy $r5 $r2
	$r3 := call $r3 $r5
	$r3 := $r2 idiv $r3
	$r0 := $r1 * $r3
	return $r0
	def L23
	$r0 := 0
	return $r0
}
setglobal $r0 "lcm"
$r0 := fun 4 {
	getglobal $r5 "list3"
	copy $r6 $r2
	copy $r7 $r3
	copy $r8 $r4
	$r5 := call $r5 $r8
	$r0 := cons $r1 $r5
	return $r0
}
setglobal $r0 "list4"
$r0 := fun 5 {
	getglobal $r6 "list4"
	copy $r7 $r2
	copy $r8 $r3
	copy $r9 $r4
	copy $r10 $r5
	$r6 := call $r6 $r10
	$r0 := cons $r1 $r6
	return $r0
}
setglobal $r0 "list5"
$r0 := fun 6 {
	getglobal $r7 "list5"
	copy $r8 $r2
	copy $r9 $r3
	copy $r10 $r4
	copy $r11 $r5
	copy $r12 $r6
	$r7 := call $r7 $r12
	$r0 := cons $r1 $r7
	return $r0
}
setglobal $r0 "list6"
$r0 := fun 7 {
	getglobal $r8 "list6"
	copy $r9 $r2
	copy $r10 $r3
	copy $r11 $r4
	copy $r12 $r5
	copy $r13 $r6
	copy $r14 $r7
	$r8 := call $r8 $r14
	$r0 := cons $r1 $r8
	return $r0
}
setglobal $r0 "list7"
$r0 := fun 8 {
	getglobal $r9 "list7"
	copy $r10 $r2
	copy $r11 $r3
	copy $r12 $r4
	copy $r13 $r5
	copy $r14 $r6
	copy $r15 $r7
	copy $r16 $r8
	$r9 := call $r9 $r16
	$r0 := cons $r1 $r9
	return $r0
}
setglobal $r0 "list8"
$r0 := fun 2 {
	$r3 := null? $r2
	if $r3 goto L24
	$r3 := car $r2
	$r4 := cdr $r2
	getglobal $r5 "equal?"
	copy $r6 $r1
	$r7 := car $r3
	$r5 := call $r5 $r7
	if $r5 goto L25
	getglobal $r5 "assoc"
	copy $r6 $r1
	copy $r7 $r4
	tailcall $r5 $r7
	def L25
	return $r3
	def L24
	$r0 := #f
	return $r0
}
setglobal $r0 "assoc"
$r0 := fun 2 {
	$r3 := null? $r1
	if $r3 goto L26
	$r3 := null? $r2
	if $r3 goto L27
	getglobal $r3 "equal?"
	$r4 := car $r1
	$r5 := car $r2
	$r3 := call $r3 $r5
	getglobal $r4 "is_prefix?"
	$r5 := cdr $r1
	$r6 := cdr $r2
	$r4 := call $r4 $r6
	$r0 := $r3 and $r4
	return $r0
	def L27
	$r0 := #f
	return $r0
	def L26
	$r0 := #t
	return $r0
}
setglobal $r0 "is_prefix?"
getglobal $r0 "is_prefix?"
$r1 := "a"
$r2 := "b"
$r3 := '()
$r2 := cons $r2 $r3
$r1 := cons $r1 $r2
$r2 := "a"
$r3 := "b"
$r4 := "c"
$r5 := '()
$r4 := cons $r4 $r5
$r3 := cons $r3 $r4
$r2 := cons $r2 $r3
$r0 := call $r0 $r2
check-assert $r0 "(is_prefix? '(a b) '(a b c))"
getglobal $r0 "is_prefix?"
$r1 := "a"
$r2 := "b"
$r3 := '()
$r2 := cons $r2 $r3
$r1 := cons $r1 $r2
$r2 := "c"
$r3 := "a"
$r4 := "b"
$r5 := '()
$r4 := cons $r4 $r5
$r3 := cons $r3 $r4
$r2 := cons $r2 $r3
$r0 := call $r0 $r2
$r0 := not $r0
check-assert $r0 "(not (is_prefix? '(a b) '(c a b)))"
getglobal $r0 "is_prefix?"
$r1 := '()
$r2 := "a"
$r3 := "b"
$r4 := "c"
$r5 := '()
$r4 := cons $r4 $r5
$r3 := cons $r3 $r4
$r2 := cons $r2 $r3
$r0 := call $r0 $r2
check-assert $r0 "(is_prefix? '() '(a b c))"
$r0 := fun 2 {
	$r3 := null? $r2
	if $r3 goto L28
	getglobal $r3 "is_prefix?"
	copy $r4 $r1
	copy $r5 $r2
	$r3 := call $r3 $r5
	getglobal $r4 "contig-sublist?"
	copy $r5 $r1
	$r6 := cdr $r2
	$r4 := call $r4 $r6
	$r0 := $r3 or $r4
	return $r0
	def L28
	$r0 := #f
	return $r0
}
setglobal $r0 "contig-sublist?"
getglobal $r0 "contig-sublist?"
$r1 := "a"
$r2 := "b"
$r3 := '()
$r2 := cons $r2 $r3
$r1 := cons $r1 $r2
$r2 := "c"
$r3 := "a"
$r4 := "b"
$r5 := '()
$r4 := cons $r4 $r5
$r3 := cons $r3 $r4
$r2 := cons $r2 $r3
$r0 := call $r0 $r2
check-assert $r0 "(contig-sublist? '(a b) '(c a b))"
getglobal $r0 "is_prefix?"
$r1 := "a"
$r2 := "b"
$r3 := '()
$r2 := cons $r2 $r3
$r1 := cons $r1 $r2
$r2 := "c"
$r3 := "b"
$r4 := "a"
$r5 := "c"
$r6 := '()
$r5 := cons $r5 $r6
$r4 := cons $r4 $r5
$r3 := cons $r3 $r4
$r2 := cons $r2 $r3
$r0 := call $r0 $r2
$r0 := not $r0
check-assert $r0 "(not (is_prefix? '(a b) '(c b a c)))"
$r0 := fun 2 {
	getglobal $r3 "is_prefix?"
	copy $r4 $r1
	copy $r5 $r2
	$r3 := call $r3 $r5
	if $r3 goto L29
	$r3 := null? $r2
	if $r3 goto L30
	getglobal $r3 "contig-sublist?"
	copy $r4 $r1
	$r5 := cdr $r2
	tailcall $r3 $r5
	def L30
	$r0 := #f
	return $r0
	def L29
	$r0 := #t
	return $r0
}
setglobal $r0 "contig-sublist?"



$r0 := fun 1 {
	getglobal $r2 "atom?"
	copy $r3 $r1
	$r2 := call $r2 $r3
	if $r2 goto L31
	getglobal $r2 "append"
	getglobal $r3 "mirror"
	$r4 := cdr $r1
	$r3 := call $r3 $r4
	getglobal $r4 "list1"
	getglobal $r5 "mirror"
	$r6 := car $r1
	$r5 := call $r5 $r6
	$r4 := call $r4 $r5
	tailcall $r2 $r4
	def L31
	return $r1
}
setglobal $r0 "mirror"
getglobal $r0 "mirror"
$r1 := "1"
$r2 := '()
$r1 := cons $r1 $r2
$r0 := call $r0 $r1
check $r0 "(mirror '(1))"
$r0 := "1"
$r1 := '()
$r0 := cons $r0 $r1
expect $r0 "'(1)"
