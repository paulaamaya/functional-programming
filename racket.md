- [Fundamentals](#fundamentals)
  - [Conditional Statements](#conditional-statements)
  - [Chaining Tests: `cond` Operator](#chaining-tests-cond-operator)
  - [Logical Operators](#logical-operators)
- [Functions](#functions)
  - [Defining Functions](#defining-functions)
  - [Bindings](#bindings)
  - [Pattern Matching](#pattern-matching)
  - [Anonymous Functions](#anonymous-functions)
  - [Higher Order Functions](#higher-order-functions)
  - [Currying](#currying)
- [Lists](#lists)
  - [List Functions](#list-functions)
  
  
# Fundamentals

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

Complex conditionals can be formed by nesting if expressions,

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

Fun fact: This can be refactored into cleaner code by using [pattern matching](#pattern-matching).

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

## Defining Functions

The syntax to define a function is as follows:

```rkt
(define (functionName <param>*...)
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

## Bindings

So far, we have mainly worked with global bindings using the syntax `(define <id> <expr>)`.  However, Racket also supports local scope - the most commom being within functions.

```rkt
(define (f x)
  (+ x 1))

(+ x 10)
; x: undefined cannot reference an identifier before its definition
```

We can also explicitly create a local binding using the keyword `let`, which follows the following syntax:

```rkt
(let* ([<id>, <expr>]*) <expr>+)
```

The nice thing about `let` is that it allows you to bind many identifiers at once, instead of requring a separate `define` for each one.
- Each binding clause is an `<id>` which gets bound to `<expr>` for use in the body.  These clauses are written inside square brackets.
- The expressions after the clauses are the body of the `let`.

```rkt
; bind x and y to a value then uses the value for the body
(let ([x (random 10)] [y (random 10)])
  ; sum is divisible by 2
  (cond [(= (modulo (+ x y) 2) 0) "2 supremacy"]
  ; sum is divisible by 3
  [(= (modulo (+ x y) 3) 0) "3 supremacy"]
  ; all other cases
  [else "LaMe"]))
```

The bindings of a `let` form are only available only in the body, so the binding clauses cannot refer to each other. The `let*` form acts in the same way, except it allows later bindings to refererence earlier bindings.

```rkt
(let* ([x (random 10)] [y (random 10)] [z (modulo x y)])
  ; sum is divisible by 2
  (cond [(= (modulo (+ x y z) 2) 0) "2 supremacy"]
  ; sum is divisible by 3
  [(= (modulo (+ x y z) 3) 0) "3 supremacy"]
  ; all other cases
  [else "LaMe"]))
```

## Pattern Matching

We can define simpler functions that leverage pattern matching using the `define/match` keyword, using the follwing syntax:

```rkt
(define/match (<id> <param>*) [(<expr>)+ expr*]*)
```

Below is an example of a function with an `if` statement that performs **control based on value**, followed by its analogous pattern matching version.

```rkt
; Value based conditional
(define (foo x)
    (cond 
    [(= x 5) 10]
    [(= x 10) 15]
    [else (+ x 13)]))

; Value based pattern matching
(define/match (foo2 x)
    [(5) 10]
    [(10) 15]
    [(_) (+ x 13)]) ; _ matches anything
```

Pattern matching can also be used on **control based on structure**, which is more commonly used in recursive definitions of functions on lists like the one below.

```rkt
; Structure based conditional
(define (listMax lst)
    (if (null? lst)
    -inf.0
    (max (first lst) (listMax (rest lst)))))

; Structutral pattern-matching
(define/match (listMax2 lst)
    [((list)) -inf.0] ; list matches the empty list
    [((cons x xs)) (max x (listMax2 xs))])
```

## Anonymous Functions

In Racket, you can use a `lambda` expression to produce a function directly. A lambda expression follows the following syntax:

```rkt
(lambda (<param>*) <expr>+)
```

If you are **only using a function once**, it is really convenient to define a `lambda` expression the moment that you need it! For instance, the example above can be simplified by,

```rkt
(twice (lambda (n) (* n 2)) 5)
; 20
```

Another use for lambdas is when you want to **define a function that produces another function**.  For example,

```rkt
; Define function that returns lambda function
(define (makePrefix s2)
    (lambda (s) (string-append s s2 )))

(define exclamation (makePrefix "!"))
(exclamation "Hello")
; "Hello!"
```


## Higher Order Functions

Consider a function that takes in another function and an argument,

```rkt
; Takes in a function and an argument, applies f two times to the argument
(define (twice f n)
    (f (f n)))

```

```rkt
; An function that we can pass to twice
(define (double n)
    (* n 2))

(twice double 5)
; 20
```

## Currying

Although not as straightforward as Haskell, currtying is still useful in Racket with the `curry` function, which turns a function `f(x1 x2 x3 ... xn)` into a function `(((((f x1) x2) x3)...)xn)`.

For instance, if we want to filter a list by elements greater than 5, we can defining skip a lambda function by using shorthand currying notation:

```rkt
(filter (curry < 5) '(1 2 3 44 55))
; '(44 55)
; The 5 always gets passed as the first arg to the binary operator <
```

You can think of currying as a way to accumulate arguments of a function until all arguments are present and `f` can be called.

> "These are the arguments I know so far, I'll give you the rest later"


# Lists

 The `list` function takes any number of values and returns a list containing the values.

 ```rkt
(list 1 2 3)
; '(1 2 3)
; A list usually prints with ' at the beginning
 ```

 Lists in Racket **need not be homogenous**, i.e. they can store values of multiple data types.

 ## List Functions

 Here are examples of a few built-in functions for lists (**Note:** None of these functions mutate the original list):

 ```rkt
(define l (list "jab" "hook" "cross"))

; first element of a list
(first l)
; "jab"

; all elements of a list but the first
(rest l)
; '("hook" "cross")

; length of a list
(length l)
; 3

; index list
(list-ref l 2)
"cross"

; append to list
(append l (list "uppercut"))
; '("jab" "hook" "cross" "uppercut")

; reverse a list
(reverse l)
; '("cross" "hook" "jab")

; check if an element is in the list
(member "weave" l)
; #f

; original list has not changed
l
; '("jab" "hook" "cross")

; create a new list using its recursive definition
(cons "uppercut" l)
; '("uppercut" "jab" "hook" "cross")
 ```

 Additionally, Racket comes with some functions that iterate over the elements of a list. The **body of a list iteration must be packaged into a function that is applied to each element**, so `lambda` expressions become super important.  

 The `map` function applies `f` to all elements of the list, and returns a new list containing each result of `f` in order.  It can take in more than one list at a time, given that the following hold:
 - The number of parameters `f` takes must be equal to the number of lists passed.
 - All lists must have the same number of elements.

```rkt
(define xs (list 1 2 3 4))
(define ys (list 10 20 30 40))

(map (lambda (n1 n2) (+ n1 n2)) xs ys)
; '(11 22 33 44)
```

The `andmap` function is equivalent to JavaScript's `every()`, while the `ormap` function is equvialent to `some()`.

```rkt
(define zs (list 1 2 3 "hello"))

(andmap number? zs)
; #f
(ormap number? zs)
; #t
```

The `filter` function only keeps elements for which the body result is true, and gets rid of those for which it is false:

```rkt
(filter number? zs)
; '(1 2 3)
```

The `foldl` function "folds" all the elements of a list from a starting point to the right according to a given function.  Unlike the other iterative functions above, a fold is not retricted to returning a list.

```rkt
(foldl - 0 (list 1 2 3 4 5))
; 3
```

Notice that this implemetation is different from Haskell's and a bit unintuitive. Read more about it [here](https://stackoverflow.com/questions/36960124/foldr-and-foldl-in-drracket).  This is an iterative implementation of Racket's `foldl` written in Python:

```py
acc = init
for x in lst:
  acc = combine(x, acc)
```

The `apply` function takes in a function `f` and a list, then applies `f` once to all elements of the list **at the same time**.  This is different from `map` which applies `f` to each item in the list individually.

```rkt
(apply + '(1 2 3 4))
; 10
; equivalent to...
(+ 1 2 3 4)
```

More generally we have that, `(apply f '(x1 x2 x3 ... xn)) == (f x1 x2 x3 ... xn)`.
