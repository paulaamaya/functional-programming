#lang racket

(define xs (list 1 2 3 4))
(define ys (list 10 20 30 40))
(define zs (list 1 2 3 "hello"))

(map (lambda (n1 n2) (+ n1 n2)) xs ys)

(andmap number? zs)
(ormap number? zs)

(define (sumList xs)
        (if (null xs)
        0
        (+ (first xs) (sumList (rest xs)))))

(define (maxList xs)
        (if (null xs) 
        -inf.0
        (max (first xs) (maxList (rest xs)))))