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
        [(make-stream) 's-null]
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

; the iterator next! macro
; takes in a thunk that evaluates to a stream
; (define-syntax next!
;     (syntax-rules ()
;         [(next! <s>)
;         (if (s-null? (<s>))
;             'DONE
;             (let* ([temp <s>])
;                 (begin
;                     (set! <s> (cdr (<s>)))
;                     (s-first (<s>)))))]))
(define-syntax next!
  (syntax-rules ()
    [(next! <g>)  ; in this version of next!, <g> is a thunk that evaluates to a stream
     (let* ([stream (<g>)]) ; first, evaluate <g>
       (if (s-null? stream)
           'DONE
           (begin
               (set! <g> (cdr stream))
               ((car stream)))))])) ; use cdr rather than s-rest

; Applies k to all elements in the stream
(define (map-stream f lst)
    (if (empty? lst)
        s-null
        (s-cons (f (first lst)) (map-stream f (rest lst)))))

; STEP 1: Assume k always return a stream
(define (s-append s t)
    (cond [(s-null? s) (t)]
    [(pair? s) (s-cons (s-first s) (s-append (s-rest s) t))]
    [else (s-cons s (t))]))

(define (s-append-map k lst)
    (if (empty? lst)
        s-null
        (s-append (k (first lst))
            (thunk (s-append-map k (rest lst))))))

; TODO: Implement singleton
(define (singleton x) (make-stream x))

; amb operator
(define (-< . options)
    (shift k (s-append-map k options)))

(define-syntax do/-<
    (syntax-rules ()
        [(do/-< <expr>) (thunk (reset (singleton <expr>)))]))

(define g (do/-< (/ 1 (-< 1 2 0 4 5))))