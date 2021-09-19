#lang racket

(define (twice f n)
    (f (f n)))

(define (double n)
    (* n 2))

(define (makePrefix s2)
    (lambda (s) (string-append s s2 )))