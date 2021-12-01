#lang racket

(define (Point x y)
    (lambda (msg)
            ;;; attributes
    (cond   [(equal? msg 'x) x]
            [(equal? msg 'y) y]
            ;;; methods
            [(equal? msg 'to-string) (lambda () (format "(~a, ~a)" x y))]
            [(equal? msg ''distance)
                (lambda (other-point)
                    (let ([dx (- x (other-point 'x))]
                          [dy (- y (other-point 'y))])
                          (sqrt (+ (* dx dx) (* dy dy)))))]
            [else "Unrecognized message"])))

(class Person
    ;;; expression listing all attributes
    (name age)
    ;; methods
    [(greet other-person)
        (string-append "Hello, " (other-person 'name) ". I am" name ".")]

    [(can-vote) (>= age 18)])