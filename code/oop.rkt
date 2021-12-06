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

; (my-class Point (x y)) should expand into a basic Point

(define-syntax my-class
    (syntax-rules (method)
        [(my-class <class-name> (<att> ...) (method (<method-name> <param> ...) <body>) ...)
        (define (<class-name> <att> ...)
            (lambda (msg)
                (cond   [(equal? msg '<att>) <att>] ...
                        [(equal? msg '<method-name>) (lambda (<param> ...) <body>)] ...
                        [else "Unrecognized message"])))]))

(my-class Point (x y)
    (method (distance other-point)
        (let* ([dx (- x (other-point 'x))]
              [dy (- y (other-point 'y))])
              (sqrt (+ (* dx dx) (* dy dy))))))