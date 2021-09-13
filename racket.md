- [Basics](#basics)
- [Functions](#functions)
  - [Function Calls](#function-calls)

# Basics

Racket values include numbers, booleans, strings, and byte strings. 

Booleans are `#t` for true and `#f` for false. In conditionals, however, all non-false values are treated as true.

The following syntax binds the first `<id>` to the result of `<expr>`.

```rkt
(define greeting "Hello!")
greeting
; "Hello!"
```

# Functions

The syntax to define a function is as follows:

```rkt
(define (functionName param1 param2...)
    (<expr>)*)
```

The function is bound to `functionName`, takes in parameters `param1 param2,...` and has a body of 0+ `(expr)`.  For example,

```rkt
(define (bake flavor)
    (printf "Preheating oven...")
    (string append ))
```

Since a function body can include multiple expressions for the function’s body, **only the value of the last expression is returned when the function is called**. The other expressions are evaluated only for some side-effect.

This can cause some problematic side-effects so Racket programmers prefer to only define functions with one expression in their body.

## Function Calls

Function calls are perfomed within parentheses - opening parentheses, calling the function, then passing parameters, all separated by spaces.

```rkt
(max 1 1.4 5)
```