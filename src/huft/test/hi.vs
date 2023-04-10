$r0 := fun 1 {
	$r2 := null? $r1
	if $r2 goto L0
	$r2 := car $r1
	$r3 := cdr $r1
	$r4 := fun 1 {
		$r2 := $r0.0
		$r0 := $r1 > $r2
		return $r0
	}
	$r4 := closure[$r4,1]
	$r4.0 := $r2
	getglobal $r5 "o"
	$r6 := fun 1 {
		$r0 := not $r1
		return $r0
	}
	copy $r7 $r4
	$r5 := call $r5 $r7
	getglobal $r6 "append"
	getglobal $r7 "qsort"
	getglobal $r8 "filter"
	copy $r9 $r5
	copy $r10 $r3
	$r8 := call $r8 $r10
	$r7 := call $r7 $r8
	getglobal $r8 "qsort"
	getglobal $r9 "filter"
	copy $r10 $r4
	copy $r11 $r3
	$r9 := call $r9 $r11
	$r8 := call $r8 $r9
	$r8 := cons $r2 $r8
	tailcall $r6 $r8
	def L0
	$r0 := '()
	return $r0
}
setglobal $r0 "qsort"
$r0 := fun 1 {
	$r2 := 0
	$r2 := $r1 = $r2
	if $r2 goto L1
	getglobal $r2 "iota^"
	$r3 := 1
	$r3 := $r1 - $r3
	$r2 := call $r2 $r3
	$r0 := cons $r1 $r2
	return $r0
	def L1
	$r0 := '()
	return $r0
}
setglobal $r0 "iota^"
getglobal $r0 "qsort"
$r1 := 65
$r2 := 15
$r3 := 87
$r4 := 42
$r5 := 62
$r6 := 45
$r7 := 6
$r8 := 81
$r9 := 53
$r10 := 34
$r11 := 33
$r12 := 82
$r13 := 79
$r14 := 7
$r15 := 17
$r16 := 39
$r17 := 71
$r18 := 18
$r19 := 98
$r20 := 92
$r21 := 77
$r22 := 41
$r23 := 51
$r24 := 16
$r25 := 86
$r26 := 30
$r27 := 49
$r28 := 10
$r29 := 4
$r30 := 68
$r31 := 35
$r32 := 52
$r33 := 69
$r34 := 12
$r35 := 85
$r36 := 36
$r37 := 47
$r38 := 5
$r39 := 1
$r40 := 61
$r41 := 74
$r42 := 64
$r43 := 31
$r44 := 80
$r45 := 25
$r46 := 29
$r47 := 93
$r48 := 78
$r49 := 72
$r50 := 24
$r51 := 99
$r52 := 48
$r53 := 76
$r54 := 19
$r55 := 66
$r56 := 70
$r57 := 3
$r58 := 56
$r59 := 23
$r60 := 32
$r61 := 84
$r62 := 100
$r63 := 91
$r64 := 58
$r65 := 20
$r66 := 60
$r67 := 26
$r68 := 37
$r69 := 97
$r70 := 54
$r71 := 46
$r72 := 13
$r73 := 21
$r74 := 63
$r75 := 28
$r76 := 14
$r77 := 59
$r78 := 67
$r79 := 38
$r80 := 88
$r81 := 57
$r82 := 40
$r83 := 55
$r84 := 94
$r85 := 11
$r86 := 95
$r87 := 22
$r88 := 44
$r89 := 27
$r90 := 9
$r91 := 83
$r92 := 50
$r93 := 43
$r94 := 8
$r95 := 90
$r96 := 73
$r97 := 75
$r98 := 96
$r99 := 89
$r100 := 2
$r101 := '()
$r100 := cons $r100 $r101
$r99 := cons $r99 $r100
$r98 := cons $r98 $r99
$r97 := cons $r97 $r98
$r96 := cons $r96 $r97
$r95 := cons $r95 $r96
$r94 := cons $r94 $r95
$r93 := cons $r93 $r94
$r92 := cons $r92 $r93
$r91 := cons $r91 $r92
$r90 := cons $r90 $r91
$r89 := cons $r89 $r90
$r88 := cons $r88 $r89
$r87 := cons $r87 $r88
$r86 := cons $r86 $r87
$r85 := cons $r85 $r86
$r84 := cons $r84 $r85
$r83 := cons $r83 $r84
$r82 := cons $r82 $r83
$r81 := cons $r81 $r82
$r80 := cons $r80 $r81
$r79 := cons $r79 $r80
$r78 := cons $r78 $r79
$r77 := cons $r77 $r78
$r76 := cons $r76 $r77
$r75 := cons $r75 $r76
$r74 := cons $r74 $r75
$r73 := cons $r73 $r74
$r72 := cons $r72 $r73
$r71 := cons $r71 $r72
$r70 := cons $r70 $r71
$r69 := cons $r69 $r70
$r68 := cons $r68 $r69
$r67 := cons $r67 $r68
$r66 := cons $r66 $r67
$r65 := cons $r65 $r66
$r64 := cons $r64 $r65
$r63 := cons $r63 $r64
$r62 := cons $r62 $r63
$r61 := cons $r61 $r62
$r60 := cons $r60 $r61
$r59 := cons $r59 $r60
$r58 := cons $r58 $r59
$r57 := cons $r57 $r58
$r56 := cons $r56 $r57
$r55 := cons $r55 $r56
$r54 := cons $r54 $r55
$r53 := cons $r53 $r54
$r52 := cons $r52 $r53
$r51 := cons $r51 $r52
$r50 := cons $r50 $r51
$r49 := cons $r49 $r50
$r48 := cons $r48 $r49
$r47 := cons $r47 $r48
$r46 := cons $r46 $r47
$r45 := cons $r45 $r46
$r44 := cons $r44 $r45
$r43 := cons $r43 $r44
$r42 := cons $r42 $r43
$r41 := cons $r41 $r42
$r40 := cons $r40 $r41
$r39 := cons $r39 $r40
$r38 := cons $r38 $r39
$r37 := cons $r37 $r38
$r36 := cons $r36 $r37
$r35 := cons $r35 $r36
$r34 := cons $r34 $r35
$r33 := cons $r33 $r34
$r32 := cons $r32 $r33
$r31 := cons $r31 $r32
$r30 := cons $r30 $r31
$r29 := cons $r29 $r30
$r28 := cons $r28 $r29
$r27 := cons $r27 $r28
$r26 := cons $r26 $r27
$r25 := cons $r25 $r26
$r24 := cons $r24 $r25
$r23 := cons $r23 $r24
$r22 := cons $r22 $r23
$r21 := cons $r21 $r22
$r20 := cons $r20 $r21
$r19 := cons $r19 $r20
$r18 := cons $r18 $r19
$r17 := cons $r17 $r18
$r16 := cons $r16 $r17
$r15 := cons $r15 $r16
$r14 := cons $r14 $r15
$r13 := cons $r13 $r14
$r12 := cons $r12 $r13
$r11 := cons $r11 $r12
$r10 := cons $r10 $r11
$r9 := cons $r9 $r10
$r8 := cons $r8 $r9
$r7 := cons $r7 $r8
$r6 := cons $r6 $r7
$r5 := cons $r5 $r6
$r4 := cons $r4 $r5
$r3 := cons $r3 $r4
$r2 := cons $r2 $r3
$r1 := cons $r1 $r2
$r0 := call $r0 $r1
check $r0 "(qsort\n   '(65\n   15\n   87\n   42\n   62\n   45\n   6\n   81\n   53\n   34\n   33\n   82\n   79\n   7\n   17\n   39\n   71\n   18\n   98\n   92\n   77\n   41\n   51\n   16\n   86\n   30\n   49\n   10\n   4\n   68\n   35\n   52\n   69\n   12\n   85\n   36\n   47\n   5\n   1\n   61\n   74\n   64\n   31\n   80\n   25\n   29\n   93\n   78\n   72\n   24\n   99\n   48\n   76\n   19\n   66\n   70\n   3\n   56\n   23\n   32\n   84\n   100\n   91\n   58\n   20\n   60\n   26\n   37\n   97\n   54\n   46\n   13\n   21\n   63\n   28\n   14\n   59\n   67\n   38\n   88\n   57\n   40\n   55\n   94\n   11\n   95\n   22\n   44\n   27\n   9\n   83\n   50\n   43\n   8\n   90\n   73\n   75\n   96\n   89\n   2))"
getglobal $r0 "reverse"
getglobal $r1 "iota^"
$r2 := 100
$r1 := call $r1 $r2
$r0 := call $r0 $r1
expect $r0 "(reverse (iota^ 100))"
