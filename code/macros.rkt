#lang racket

(define-syntax defs
    (syntax-rules (defs)
    [(defs) (begin 
        (define x 1)
        (define y (+ 10))
        (+ x y))]))

(define-syntax my-and
    (syntax-rules ()
    [(my-and) (void)]
    [(my-and <bool>) <bool>]
    [(my-and <bool> <next-bools> ...) (and <bool> (my-and <next-bools> ...))]))

(define-syntax my-cond
    (syntax-rules (else)
    [(my-cond) (void)]
    [(my-cond [else e1]) e1]
    [(my-cond [<pred> <e1>] <other-pairs> ...) (if <pred> <e1> (my-cond <other-pairs> ...))]))
