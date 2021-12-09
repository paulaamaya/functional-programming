#lang racket

(require racket/control)

; Empty stream value
(define s-null 's-null)

; Check for empty stream
(define (s-null? s) (equal? s s-null))

; Creates a stream whose first value is <first> and whose other items are in <rest>
; GOAL: (s-cons <first> <rest>) -> stream
; <first> : any
; <rest> : stream
(define-syntax s-cons
    (syntax-rules ()
        [(s-cons <first> <rest>)
        (cons (thunk <first>) (thunk <rest>))]))

; car is general form of (first)
(define (s-first s) ((car s)))
; cdr is general from of (rest)
(define (s-rest s) ((cdr s)))

; Creates a stream containing the given values
; GOAL: (make-stream <expr> ...) -> stream
(define-syntax make-stream
    (syntax-rules ()
        [(make-stream <expr>) (s-cons <expr> 's-null)]
        [(make-stream <expr> <other-expr> ...)
        (s-cons <expr> (make-stream <other-expr> ...))]))

; Returns a list containing the values in this stream
; GOAL: (stream->list s) -> list
(define (stream->list s)
    (if (s-null? s)
        null
        (cons (s-first s) (s-rest s))))

; Returns a stream containing the numbers [start, end).  Empty stream if start >= end
; GOAL: (s-range start end) -> stream
(define (s-range start end)
    (if (>= start end)
        s-null
        (s-cons start (s-range (+ start 1) end))))

; Returns a new stream that returns the first <n> elems of <stream>, or all elements
; if stream has less than n elements
; GOAL: (s-take s n) -> stream
(define (s-take s n)
    (cond   [(s-null? s) s-null]
            [(equal? n 0) s-null]
            [else (s-cons (s-first s) (s-take (s-rest s) (- n 1)))]))


; Applies k to all elements in the stream
(define (map-stream k lst)
    (if (empty? lst)
        s-null
        (s-cons (k (first lst)) (map-stream k (rest lst)))))

; the iterator next! macro
(define-syntax next!
    (syntax-rules ()
        [(next! <s>)
        (if (s-null? <s>)
            'DONE
            (let* ([temp <s>])
                (begin
                    (set! <s> (s-rest <s>))
                    (s-first temp))))]))

; naive amb operator
(define (-< . lst)
    (shift k (map-stream k lst)))

(define g (reset (+ 1 (-< 3 4))))