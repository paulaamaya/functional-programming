#lang racket 

(define/match (num-plus datum)
    [((list expr)) (apply + (map num-plus expr))]
    [('+) 1]
    [(_) 0])

; interpreter for the lambda calculus
(define/match (lambdaInterp expr)
    ; function call
    [((list f arg))  null]
    ; function expression
    [((list 'lambda (list param) body)) expr]
    ; identifier
    [(id) id])