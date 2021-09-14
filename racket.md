- [Basics](#basics)
  - [Conditional Statements](#conditional-statements)
  - [Chaining Tests: `cond` Operator](#chaining-tests-cond-operator)
  - [Logical Operators](#logical-operators)
- [Functions](#functions)
  - [Anonymous Functions](#anonymous-functions)

# Basics

Racket values include numbers, booleans, strings, and byte strings. Booleans are `#t` for true and `#f` for false. In conditionals, however, all non-false values are treated as true.

The following syntax binds the first `<id>` to the result of `<expr>`.  This is one use of the keyword `define`, to bind variables to a value.

```rkt
(define greeting "Hello!")
greeting
; "Hello!"
```

## Conditional Statements

An if-statement follows the following syntax:

```rkt
(if <expr1> <expr2> <expr3>)
```
The first `<expr1>` is always evaluated. If it evaluates to a non-`#f` value, then `<expr2>` is evaluated for the result of the whole if expression. Else if `<expr1>` evaluates to `#f`, then `<expr3>` is evaluated for the result.

```rkt
(if (> 2 3)
    "2 is bigger than 3"
    "2 is smaller than 3")
; "2 is smaller than 3"
```

Complex conditionals can be formed by nesting if expressions.  Although this can be refactored into cleaner code, here is an example,

```rkt
(define (reply s)
  (if (string? s)
      (if (string-prefix? s "hello")
          "hello back!"
          "huh?")
      "huh?"))

(reply "hello world!")
; "hello back!"
(reply "what's up?")
; "huh?"
```

## Chaining Tests: `cond` Operator

Another common use for conditional statments is a sequence of tests, each with its own result.

```rkt
(define (helloSpecies s)
  (if (string-prefix? s "human")
      "Hello human!"
      (if (string-prefix? s "droid")
          "Hello droid!"
          (if (string-prefix? s "meeseek")
              "Hello meeseek!"
              "I don't know your species!"))))
```

The shorthand for a sequence of tests is the `cond` operator, which follows the following syntax:

```rkt
(cond [<expr1> <expr>*]*)
```

A `cond` operator takes in a sequence of clauses in square brackets.
- In each clause `<expr1>` is a test expression.  If `<expr1>` evaluates to `#t`, then the clause's remaining `<expr>` are evaluated.  The last expression in the clause provides the answer for the entire `cond` expression.
- If the test `<expr1>` produces `#f`, then the clause’s remaining `‹expr›`s are ignored, and evaluation continues with the next clause.
- The last clause can use `else` as `<expr1>`, for a `#t` test expression that always holds.

So instead of using nested if-statemnts, we can rewrite the example above as:

```rkt
(define (helloSpecies2 s)
  (cond [(string-prefix? s "human") "Hello human!"]
        [(string-prefix? s "droid") "Hello droid!"]
        [(string-prefix? s "meeseek") "Hello meeseeks!"]
        [else "I don't know your species!"]))
```

## Logical Operators
To make your statements more readable, you can use Racket's logical operators which follow the following syntax:

```rkt
(and <expr> <expr> ...)
(or <expr> <expr> ...)
```

The `and` short-circuits, it stops and returns `#f` when an expression produces `#f`. The `or` similarly short-circuits when it encounters a true result.

```rkt
(define (reply s)
  (if (and string? s string-prefix? s "hello")
      "hello back!"
      "huh?"))
```

Furthermore, both logical operators work with any number of expressions.

# Functions

Function calls are perfomed within parentheses - opening parentheses, calling the function, then passing parameters, all separated by spaces.

```rkt
(max 1 1.4 5)
; 5.0
(+ 1 2)
; 3
(>= 2 1)
; #t
```

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

## Anonymous Functions

In Racket, you can use a `lambda` expression to produce a function directly. A lambda expression follows the following syntax.