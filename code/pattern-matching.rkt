(define/match (factorial n)
    [(0) 1]
    [(_) (* n (factorial (- n 1)))])

; Value based conditional
(define (foo x)
    (cond 
    [(= x 5) 10]
    [(= x 10) 15]
    [else (+ x 13)]))

; Value based pattern matching
(define/match (foo2 x)
    [(5) 10]
    [(10) 15]
    [(_) (+ x 13)]) ; _ matches anything

; Structure based conditional
(define (listMax lst)
    (if (null? lst)
    -inf.0
    (max (first lst) (listMax (rest lst)))))

; Structutral pattern-matching
(define/match (listMax2 lst)
    [((list)) -inf.0] ; list matches the empty list
    [((cons x xs)) (max x (listMax2 xs))])

; An interpreter for a binary arithmetic expression
(define/match (calculate expr)
    ; binary operation
    [((list op e1 e2))
    (let* ([val1 (calculate e1)] [val2 (calculate e2)])
    (cond 
    [(equal? op '+) (+ val1 val2)]
    [(equal? op '-) (- val1 val2)]
    [(equal? op '*) (* val1 val2)]
    [(equal? op '/) (/ val1 val2)]))]
    ; number
    [(num) num])