;; shrinked version of predef functions such that it does not segfuaults

;  predefined uScheme functions 96a 
(define caar (xs) (car (car xs)))
(define cadr (xs) (car (cdr xs)))
(define cdar (xs) (cdr (car xs)))
;  predefined uScheme functions ((elided)) (THIS CAN'T HAPPEN -- claimed code was not used) 
;  more predefined combinations of [[car]] and [[cdr]] S151b 
(define cddr  (sx) (cdr (cdr  sx)))

(define list1 (x)     (cons x '()))
(define list2 (x y)   (cons x (list1 y)))
(define list3 (x y z) (cons x (list2 y z)))
;  predefined uScheme functions 99b 
(define append (xs ys)
  (if (null? xs)
     ys
     (cons (car xs) (append (cdr xs) ys))))

;  predefined uScheme functions 101a 
(define reverse (xs) (revapp xs '()))


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
;; ;  predefined uScheme functions 105c 
(define mk-alist-pair (k a) (list2 k a))
;; (define alist-pair-key        (pair)  (car  pair))
;; (define alist-pair-attribute  (pair)  (cadr pair))
;; ;  predefined uScheme functions 105d 
;; (define alist-first-key       (alist) (alist-pair-key       (car alist)))
;; (define alist-first-attribute (alist) (alist-pair-attribute (car alist)))
;; ;  predefined uScheme functions 106a 
(define bind (k a alist)
  (if (null? alist)
    (list1 (mk-alist-pair k a))
    (if (equal? k (alist-first-key alist))
      (cons (mk-alist-pair k a) (cdr alist))
      (cons (car alist) (bind k a (cdr alist))))))
;; (define find (k alist)
;;   (if (null? alist)
;;     '()
;;     (if (equal? k (alist-first-key alist))
;;       (alist-first-attribute alist)
;;       (find k (cdr alist)))))
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
;; (define <= (x y) (not (> x y)))
;; (define >= (x y) (not (< x y)))
;; (define != (x y) (not (= x y)))
;; ;  predefined uScheme functions S150e 
;; (define max (x y) (if (> x y) x y))
;; (define min (x y) (if (< x y) x y))
;; ;  predefined uScheme functions S151a 
;; (define negated (n) (- 0 n))
;; (define mod (m n) (- m (* n (idiv m n))))
;; (define gcd (m n) (if (= n 0) m (gcd n (mod m n))))
;; (define lcm (m n) (if (= m 0) 0 (* m (idiv n (gcd m n)))))
;; ;  predefined uScheme functions S151e 
