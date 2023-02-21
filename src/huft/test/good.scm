;; x, v, @(x₁, ..., xₙ), x(x₁, ..., xₙ), if x then e₁ else e₂,
;; let x = e in e', (e₁; e₂), x := e, and while x := e do e'
x
'helloworld

(let* ([x 1] 
       [y (let ([z 2]) (+ x z))]) y)

(define f (x) (let ([y 1]) (+ x y)))

(let* ([x 1]
       [y f])
       (y x)
)

(let ([x 1])
      (if x 1 2))

(begin 1 2)

(while (let ([x 1]) x) 1)

(let ([x 1]) (set y x))