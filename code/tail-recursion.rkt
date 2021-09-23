; traditional recursive definition
(define (sum lst)
    (if (empty? lst)
    0
    (+ (first lst) (sum (rest lst)))))

; tail call recursive definition
(define (sum-tail lst)
    (sum-helper lst 0))

(define (sum-helper lst agg)
    (if (empty? lst)
    agg
    (sum-helper (rest lst) (+ agg (first lst)))))


(define (factorial n)
    (factorial-helper n 1))

(define/match (factorial-helper n agg)
    [(0 agg) agg]
    [(n agg) (factorial-helper (- n 1) (* n agg))])