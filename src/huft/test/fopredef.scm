;  predefined uScheme functions 96a 
(define caar (xs) (car (car xs)))
(define cadr (xs) (car (cdr xs)))
(define cdar (xs) (cdr (car xs)))
;  predefined uScheme functions ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) 
;  more predefined combinations of [[car]] and [[cdr]] S151b 
(define cddr  (sx) (cdr (cdr  sx)))
(define caaar (sx) (car (caar sx)))
(define caadr (sx) (car (cadr sx)))
(define cadar (sx) (car (cdar sx)))
(define caddr (sx) (car (cddr sx)))
(define cdaar (sx) (cdr (caar sx)))
(define cdadr (sx) (cdr (cadr sx)))
(define cddar (sx) (cdr (cdar sx)))
(define cdddr (sx) (cdr (cddr sx)))
;  more predefined combinations of [[car]] and [[cdr]] S151c 
(define caaaar (sx) (car (caaar sx)))
(define caaadr (sx) (car (caadr sx)))
(define caadar (sx) (car (cadar sx)))
(define caaddr (sx) (car (caddr sx)))
(define cadaar (sx) (car (cdaar sx)))
(define cadadr (sx) (car (cdadr sx)))
(define caddar (sx) (car (cddar sx)))
(define cadddr (sx) (car (cdddr sx)))
;  more predefined combinations of [[car]] and [[cdr]] S151d 
(define cdaaar (sx) (cdr (caaar sx)))
(define cdaadr (sx) (cdr (caadr sx)))
(define cdadar (sx) (cdr (cadar sx)))
(define cdaddr (sx) (cdr (caddr sx)))
(define cddaar (sx) (cdr (cdaar sx)))
(define cddadr (sx) (cdr (cdadr sx)))
(define cdddar (sx) (cdr (cddar sx)))
(define cddddr (sx) (cdr (cdddr sx)))
;  predefined uScheme functions 96b 
(define list1 (x)     (cons x '()))
(define list2 (x y)   (cons x (list1 y)))
(define list3 (x y z) (cons x (list2 y z)))
;  predefined uScheme functions 99b 
(define append (xs ys)
  (if (null? xs)
     ys
     (cons (car xs) (append (cdr xs) ys))))
;  predefined uScheme functions 100b 
(define revapp (xs ys) ; (reverse xs) followed by ys
  (if (null? xs)
     ys
     (revapp (cdr xs) (cons (car xs) ys))))
;  predefined uScheme functions 101a 
(define reverse (xs) (revapp xs '()))
;  predefined uScheme functions ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) 
(define nth (n xs)
  (if (= n 0)
    (car xs)
    (nth (- n 1) (cdr xs))))

(define CAPTURED-IN (i xs) (nth (+ i 1) xs))
;  definitions of predefined uScheme functions [[and]], [[or]], and [[not]] 97a 
(define and (b c) (if b  c  b))
(define or  (b c) (if b  b  c))
(define not (b)   (if b #f #t))
;  predefined uScheme functions 102c 
(define atom? (x) (or (symbol? x) (or (number? x) (or (boolean? x) (null? x)))))
;  predefined uScheme functions 103b 
(define equal? (sx1 sx2)
  (if (atom? sx1)
    (= sx1 sx2)
    (if (atom? sx2)
        #f
        (and (equal? (car sx1) (car sx2))
             (equal? (cdr sx1) (cdr sx2))))))
;  predefined uScheme functions 105c 
(define make-alist-pair (k a) (list2 k a))
(define alist-pair-key        (pair)  (car  pair))
(define alist-pair-attribute  (pair)  (cadr pair))
;  predefined uScheme functions 105d 
(define alist-first-key       (alist) (alist-pair-key       (car alist)))
(define alist-first-attribute (alist) (alist-pair-attribute (car alist)))
;  predefined uScheme functions 106a 
(define bind (k a alist)
  (if (null? alist)
    (list1 (make-alist-pair k a))
    (if (equal? k (alist-first-key alist))
      (cons (make-alist-pair k a) (cdr alist))
      (cons (car alist) (bind k a (cdr alist))))))
(define find (k alist)
  (if (null? alist)
    '()
    (if (equal? k (alist-first-key alist))
      (alist-first-attribute alist)
      (find k (cdr alist)))))
;  predefined uScheme functions 125a 
;  predefined uScheme functions 126b 
;  predefined uScheme functions 129a 
(define filter (p? xs)
  (if (null? xs)
    '()
    (if (p? (car xs))
      (cons (car xs) (filter p? (cdr xs)))
      (filter p? (cdr xs)))))
;  predefined uScheme functions 129b 
(define map (f xs)
  (if (null? xs)
    '()
    (cons (f (car xs)) (map f (cdr xs)))))
;  predefined uScheme functions 130a 
(define app (f xs)
  (if (null? xs)
    #f
    (begin (f (car xs)) (app f (cdr xs)))))
;  predefined uScheme functions 130b 
(define exists? (p? xs)
  (if (null? xs)
    #f
    (if (p? (car xs)) 
      #t
      (exists? p? (cdr xs)))))
(define all? (p? xs)
  (if (null? xs)
    #t
    (if (p? (car xs))
      (all? p? (cdr xs))
      #f)))
;  predefined uScheme functions 130d 
(define foldr (op zero xs)
  (if (null? xs)
    zero
    (op (car xs) (foldr op zero (cdr xs)))))
(define foldl (op zero xs)
  (if (null? xs)
    zero
    (foldl op (op (car xs) zero) (cdr xs))))
;  predefined uScheme functions S150c 
(val newline      10)   (val left-round    40)
(val space        32)   (val right-round   41)
(val semicolon    59)   (val left-curly   123)
(val quotemark    39)   (val right-curly  125)
                        (val left-square   91)
                        (val right-square  93)
;  predefined uScheme functions S150d 
(define <= (x y) (not (> x y)))
(define >= (x y) (not (< x y)))
(define != (x y) (not (= x y)))
;  predefined uScheme functions S150e 
(define max (x y) (if (> x y) x y))
(define min (x y) (if (< x y) x y))
;  predefined uScheme functions S151a 
(define negated (n) (- 0 n))
(define mod (m n) (- m (* n (idiv m n))))
(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))
(define lcm (m n) (if (= m 0) 0 (* m (idiv n (gcd m n)))))
;  predefined uScheme functions S151e 
(define list4 (x y z a)         (cons x (list3 y z a)))
(define list5 (x y z a b)       (cons x (list4 y z a b)))
(define list6 (x y z a b c)     (cons x (list5 y z a b c)))
(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))
(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))
(define assoc (v pairs)
  (if (null? pairs)
      #f
      (let* ([first (car pairs)]
            [rest (cdr pairs)])
        (if (equal? v (car first))
            first
            (assoc v rest)))))

(define Table.new ()
  (cons nil '()))

(define Table.get (t k)
  (let ([pair (assoc k (cdr t))])
    (if pair
        (cdr pair)
        nil)))

(define Table.put (t k v)
  (let ([pair (assoc k (cdr t))])
    (if pair
        (set-cdr! pair v)
        (set-cdr! t (cons (cons k v) (cdr t))))))
