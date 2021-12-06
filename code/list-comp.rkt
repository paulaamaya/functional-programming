#lang racket

;;; GOAL: (list-comp (+ x 2) : x <- (list 1 2 3 4))
(map (lambda (x) (+ x 2)) (list 1 2 3 4))

;;; GOAL: (list-comp <out-expr> : <id> <- <list-expr>)
;;; MAP:  (map (lambda (<id>) <out-expr>) <list-expr>)

(define-syntax  list-comp                                   ;Name of macro
    (syntax-rules (: <- if)                                 ;Literal keywords
        [(list-comp <out-expr> : <id> <- <list-expr>)       ;Pattern 
            (map (lambda (<id>) <out-expr>) <list-expr>)]   ;Template
        [(list-comp <out-expr> : <id> <- <list-expr> if <condition> ...)
            (map (lambda (<id>) <out-expr>)
            ; filter the list to map across first 
            (filter (lambda (<id>) (and <condition> ...)) <list-expr>))]))