#lang racket

; without macros we hard code for each class
(define (Point x y)
    (lambda (msg)
            ; attributes
    (cond   [(equal? msg 'x) x]
            [(equal? msg 'y) y]
            ; methods
            [(equal? msg 'to-string) (lambda () (format "(~a, ~a)" x y))]
            [(equal? msg ''distance)
                (lambda (other-point)
                    (let ([dx (- x (other-point 'x))]
                          [dy (- y (other-point 'y))])
                          (sqrt (+ (* dx dx) (* dy dy)))))]
            [else "Unrecognized message"])
    )
)

; GOAL:
; (class Person
;     ; expression listing all attributes
;     (name age)
;     ; methods
;     [(greet other-person)
;         (string-append "Hello, " (other-person 'name) ". I am" name ".")]

;     [(can-vote) (>= age 18)])

; (my-class Point (x y)) should expand into a basic Point
(define-syntax my-class
    (syntax-rules (method)
        [(my-class <class-name> 
        ; ellipsis paired with <att>
        (<att> ...)
        ; ellipsis paired with <param>
        (method (<method-name> <param> ...) <body>) 
        ; ellipsis paired with all the method pattern above
        ...) 
        (lambda (msg)
            (cond   [(equal? msg (quote <att>)) <att>]
                    [(equal? msg (quote <method-name>)) (lambda (<param> ...) <body>)]
                    [else "Unrecognized message!"]))]))