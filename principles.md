- [Programming Languages](#programming-languages)
  - [Abstract Syntax Trees (ASTs)](#abstract-syntax-trees-asts)
  - [Semantics](#semantics)
- [Lambda Calculus](#lambda-calculus)
  - [Referential Transparency](#referential-transparency)
- [Lists and Structural Recursion](#lists-and-structural-recursion)
- [Tail Recursion](#tail-recursion)
- [Programming with ASTs](#programming-with-asts)
  - [Racket: Quoted Expressions](#racket-quoted-expressions)
  - [Haskell: Value Constructors](#haskell-value-constructors)
- [Operational Semantics](#operational-semantics)
  - [Strict Evaluation Semantics in Racket](#strict-evaluation-semantics-in-racket)
    - [Non-Strict Syntactic Forms](#non-strict-syntactic-forms)
    - [Delaying Evaluation](#delaying-evaluation)
  - [Non-Strict Semantics in Haskell](#non-strict-semantics-in-haskell)
  - [Lexical Closures](#lexical-closures)
- [Type Systems](#type-systems)
- [Streams](#streams)
  - [Self-Updating Streams](#self-updating-streams)
- [Continuation](#continuation)
- [Appendix](#appendix)
  - [A New Approach to OOP](#a-new-approach-to-oop)
  - [The Ambiguous Choice Operator `-<`](#the-ambiguous-choice-operator--)
    - [Branching Choices](#branching-choices)

---

# Programming Languages

**Syntax:** Set of rules governing the form of *allowed expressions* in a programming language.

**Grammar:** Formal description of how to *generate expressions* by substitution.

**Semantics:** Rules governing the *meaning* of programs written using the grammar.

**Non-Terminal Symbols:** Symbols that **can be substituted**, also known as *syntactic variables*.  A grammar includes a start symbol, which is a designated member from the set of non-terminal symbols from which all strings in the language may be derived by sucessive applications of production rules.

**Terminal Symbol:** Literal symbols that may appear in the outputs of the production rules of a grammar and **cannot be futher changed** by applying the rules of the grammar.

Consider a simple grammar defined by two rules:

1. The symbol $\daleth$ can become $\beth \alef$
2. The symbol $\daleth$ can become $\beth$

Here $\daleth$ is a non-terminal symbol because it has two grammar rules that can change it.  Meanwhile, $\beth$ is a terminal symbol since there is no rule that can change it into something else.

> The language defined by a grammar is precisely the set of terminal strings that can be derived by applying the rules of the grammar recusively to a source string.

An expression is **syntactically valid** $\iff$ it can be generated by the language's grammar.

## Abstract Syntax Trees (ASTs)

Because the source code can only represent a program if it is syntactically valid, parsing is always informed by the language's grammar.

**Abstract Syntax Trees (ASTs)** are a tree representation of the abstract synctactic structure of a program.  They usually all share the following properties:

1. A **leaf** is an expression that has no subexpressions (i.e. an *atomic* expression)
  - Literal values (`5, "hello"`)
  - Identifiers (`person, x`)
2. Every **internal node** represents a *compound expression*.
  - Control flow stuctures
  - Function definitions & calls
  - Arithmetic operations
3. Nodes can be categorized by the kind of expression they represent through,
  - A "tag" string
  - A `Node` class hierarchy
  - Algeraic data types
4. Node types are usually in rough correspondence to the grammar rules of the language.  We can talk about,
  - Literal value nodes
  - Function definition nodes with children for the name, parameters, and body of the function.

## Semantics

In imperative programming languages, we need to define the semantics (i.e the meaning) of individual expressions and other keywords such as `return`, `for`, etc.  In the funcional programming model, **a program is just an expression** - telling the computer to *run the program is equivalent to telling it to evaluate the expression*.

**Denotational Semantics:** An approach to formalizing the meanings a programming languages by constructing **mathematical objects (called denotations) that describe the meanings of expressions**.  For example, all the expressions below have the same denotational (or mathematical) meaning:

```py
10
3 + 7
(lambda x: x + 3)(7)
```

**Operational Semantics:** These apply meaning to expressions by focusing on execution and procedures, rather than by attaching mathematical meanings to its terms.

**Axiomatic Semantics:** Apply meaning to expressions by effect on assertions about the program state. The assertions are predicates with variables, where the variables define the state of the program (e.g. variants, invariants, etc).

---

# Lambda Calculus

Lambda calculus is a formal system for expressing computation based on function defintion and application, using variable binding and substitution.  In this model, **expressions are the fundamental and only units of computation**.

It consists of constructing lambda terms and performing reduction operations on them.  Thus, evaluating an expression means performing simplifications until it can no longer be simplified.

In its basic form, an expression is one of the following:
1. An **identifier** - A character or string representing a parameter, mathematical,or logical value (e.g. $x$).
2. A **function expression** $\lambda x . M$ - Function definition where $x$ becomes bound in the expression and $M$ is a lambda term (e.g. the identity function $\lambda x . x$).
3. A **function application** $f\ N$ - This expression applies the function $f$ to lambda expression $N$.

These functions have no concept of time or external state that can affect them.  The only thing we can do is susbtitute the arguments into the function, evaluate the body, and produce a single value.

```racket
(lambda (<param> ...) <body>)
```

```haskell
\ <param> ... -> <body>
```

```py
lambda <param> ... : <body>
```

In programming we say that a **mathematical pure function** is one that meets the following conditions:

1. The function's behaviour is *exactly determined by the value of its inputs*.  Pure functions must be deterministic.

> A **deterministic system** one where no randomness is involved in the development of future states of the system - thus it will always provide the same output from an initial state.

2. The function only returns a value, they have no "side effects".

## Referential Transparency

Much like in mathematics, identifier bindings in pure functional programming are immutable.  Thus, we say that an identitfier is **referentially transparent** if it can be substituted with its value in the source code without changing the meaning of the program.

---

# Lists and Structural Recursion

Imperative languages naturally process lists uaing loops, but pure functional programming does not permit the mutation of an index identifier to process each element one at a time.  So we define lists recursively as:
1. The empty list is a list.
2. If `x` is a value and `lst` is a list, we can create a new list by combining `x` with the elements of `lst`.

The grammar for creating a list in **Racket** looks like this:

```
<list> =  'null'| '(' 'cons' <expr> <list> ')'
```

While the grammar for creating a list in **Haskell** is:

```
<list> = '[]' | <expr> ':' <list>
```

This recursive definition of lists tells us, not only about their representation, but also how to operate on them (Hint: It's recursive!).  We want to operate on the first, element of the list then recusively operate on the rest.

---

# Tail Recursion

Tail recursion is a technique to optimize recursive functions by **making the recursive call the last thing you do**, i.e. the recursion is the *tail call*.  [This](https://www.youtube.com/watch?v=_JtPhF8MshA) is a great video that explains why tail recursion is so much more efficient.

> A **tail call** is a **subroutine call performed as the final action of a procedure**.  When the subroutine is identical to the procedure, we refer to the subroutine as tail recursive.

Haskell and Racket take advantage of tail recursion: when it calls a function that is in tail call position (this is detected by the compiler), it first **removes the calling function's stack frame, and this results in constant stack height**.

```rkt
; traditional recursive definition
(define (sum lst)
    (if (empty? lst)
    0
    (+ (first lst) (sum (rest lst)))))

; tail call recursive definition
(define (sum-tail lst)
    (sum-helper lst 0))

(define (sum-helper lst agg)
    (if (empty? lst)
    agg
    (sum-helper (rest lst) (+ agg (first lst)))))
```

The evaluation of `(sum-tail '(1 2 3 4))` produces the function calls `(sum-helper '(1 2 3 4) 0)`, `(sum-helper '(2 3 4) 1)`, `(sum-helper '(3 4) 3)`, etc.  Since Racket performs tail call elimination, each of these function calls replaces the stack frame for the one before it. So the sequence,

```rkt
(sum-helper '(1 2 3 4) 0)
(sum-helper '(2 3 4) 1)
(sum-helper '(3 4) 3)
(sum-helper '(4) 6)
(sum-helper '() 10)
```

can really be viewed, not as five function calls, but as an iterative process that executes the function body and updates the variables `lst` and `agg` with every iteration.

---

# Programming with ASTs

We can generalize the *list* data structure into the *tree* data structure, where **the recursive part of the structure contains an arbitrary number of subcomponents** rather than just one.

## Racket: Quoted Expressions

Since Racket is a Lisp descendant, **the parenthesization of source code immediately creates a nested list structure**, i.e. a tree.  We call this *quoting an expression*.

```rkt
; a regular racket expression 
(+ 1 2)
; 3

; a list of three elements
(first '(+ 1 2))
; '+

; this is a nested list
'((+ 1 2) (* 4 5))
```

Fomally, here are the types of values that make up the tree strucutre in a Racket datum:

1. **Literal to literal** - Quoted literals (e.g. ints, bools, strings) represent the same values in the tree that they do in the source code.

```rkt
(equal? '#t #t)
; #t

(equal? '"hello" "hello")
; #t
```

2. **Keyword to symbol** -  Quoted identifiers or keywords become symbols in the tree and can be used for pattern-matching.

3. Quoted compound expressions become lists, in which each element is the quoted version of the corresponding subexpression.

Here is an example of a function that takes in a datum and returns the number of times the identifier `+` appears in the datum:

```rkt
(define/match (num-plus datum)
    [((list expr ...)) (apply + (map num-plus expr))]
    [('+) 1]
    [(_) 0])
```

## Haskell: Value Constructors

Since Haskell lists must be homogenous, we cannot store a list with nested lists, symbols and literals all mixed in.

However, its type system does allow us to to declare and pattern match on a type with multiple constructors.

```hs
data Expr = 
    -- An integer literal
    NumLiteral Int 
    -- An identifier
    | Identifier String
    -- A function call where
        -- Expr is the function being called
        -- [Expr contains the arguments]
    | Call Expr [Expr] -- A function call
```

The main point is that `NumLiteral`,`Identifier`, and `Call` are *value constructors* that take in a type and return a value of type `Expr`.  Notice how the structure is in line with the [definition](#abstract-syntax-trees-asts) of an AST.

Here is the implementation in Haskell of the `numPlus` function we wrote above in Racket:

```hs
numPlus :: Num a => Expr -> a
numPlus (Call f args) = numPlus f + sum (map numPlus args)
numPlus (Identifier "+") = 1
numPlus (Identifier _) = 0
numPlus (NumLiteral _) = 0
```

---

# Operational Semantics

Operational sematics are not concerned with an expressions final value after evaluation, but rather *how the expression is evaluated*.  This concept becomes important for complex expressions with lots of sub-nested expressions.  Intuitively, the order of evaluation could potentially result in one expression having two final values.

It turns out all roads lead to Rome.  The **Church-Rosser Theorem** says that for any **valid program** in the lambda calculus, every possible order of fuction application must result in the same value.

But what happens with invalid programs?  It is in aswering this question that we must discuss *evaluation order*.

**Undefined Value:** An invalid expression results in an undefined value if it represents a computation that does not complete - due to its abstract mathematical meaning or because it represents non-terminating computation.

## Strict Evaluation Semantics in Racket

**Strict Evaluation Denotational Semantics:** We say that a language has SEDS if and only if whenever an expression contains a subexpression whose value is undefined, the value of the expression itself is also undefined.

In this context, an expression contains two subexpressions: the function being called, and the arguments of which to apply the function.

**Left-to-Right Eager Evaluation:** A common implementation of SEDS, which evaluates function calls in the following way:

1. Evaluate the subexpression representing the function being called (commonly the indetifier).
2. Evaluate each argument subexpression, left-to-right.
3. Substitiute the *value* of each argument subexpression into the body of the function.

Since the arguments are evaluated before being substituted into the body of a function, if any of them (or even the function expression) are undefined, this is discovered before the function call.

### Non-Strict Syntactic Forms

We know that some expressions do not eagerly evaluate all of their subexpressions, and instead short-circuit (e.g. `and`, `or`, conditionals).  These are **synctactic forms**, rather than identifiers that refer to built-in functions.

For instance, the following two expressions evaluate to the same value, but they are not indentical because of evaluation order.

```rkt
(define (my-and x y) (and x y))

; SYNTACTIC FORM
(and #f (/1 0))     ; evaluates to #f

; FUNCTION CALL
(my-and #f (/1 0))  ; raises error
```

### Delaying Evaluation

In general, the body of a function is not evaluated until the function is called.  This means that we can delay the evauation of an expression, simply by putting it inside the body of a function.

**Thunk:** A nullary function whose purpose is to delay the evaluation of an expression in its body.  

For instance, `(lambda () (/ 1 0))` is a thunk, wrapping the error-raising expression.

```
; THUNK
(define (bad) (/ 1 0))

bad
; #<procedure:bad>

(bad)
; /: division by zero
```

Using this feature we can simulate non-strict semantics by passing in thunks that wrap the arguments.

```
(define (my-and x y)
  (and (x) (y)))

; Pass in thunks instead of expressions
(my-and (lambda () #f) (lambda () (/ 1 0)))
; (/ 1 0) is never evaluated
#f  
```

## Non-Strict Semantics in Haskell

**Lazy Evaluation:** Evaluation strategy which delays the evaluation of an expression until its value is needed.  It is also called call-by-need evaluation.

Haskell uses lazy evaluation for both function expressions and arguments.  For instance in the function `onlyFirst x y = x`, the parameter `y` is never actually evaluated - so we could technically pass it something crazy!

A fun fact is that **all name binding are thunks**, i.e. in the binding `<id> = <expr>`, the expression `<expr>` is really the body of the thunk.  So it does not get evaluated until the thunk is called.

```hs
-- No error when x is bound
x = error "This is an error"

-- Error only when x is actually evaluated
x
-- ***Exception: This is an error
```

Lazy evaluation has its caveats in predicting how a function will really behave.  Consider the case of `foldl`, which loses a lot of its tail-recursive efficiency:

```hs
foldl _ acc [] = acc
foldl f acc (x:xs) = 
  let acc' = f acc x
  in foldl f acc' xs
```

Because of lazy evaluation, the `acc'` expressions are subnested into each other and take up more space than an accumulator integer would.  Futhermore, these accumulator thunks are only called when we reach the end of the list!

We can force the evaluation of an expression into a **strict parameter** even when not "necessary" with a compiler extension as follows,

```hs
{-# LANGUAGE BangPatterns #-}

-- Strict param denoted by !
foldl _ !acc [] = acc
foldl f !acc (x:xs) = 
  let acc' = f acc x
  in foldl f acc' xs
```

## Lexical Closures

**Free Variable:** An identifier in a function is said to be a free variable if its used locally, but defined in an enclosing scope of the function.

**Closure:** A record that stores a function, along with an environment.  The environment is a mapping associating each free variable of the function with the *value or reference to which the name was bound when the closure was created*. In particular, **a closure is a value** and not an expression.

In the following example, each of the three `print_i` functions in `flist` is storing a reference and not a value.  Thus they are all pointing to the address of the variable `i` (which terminates as 3) and so they all store the same address in the closure.

```py
def make_functions():
  flist = []
  for i in [1,2,3]:
    # II. The closure of f has stored a pointer to i.  But i=3 after the loop ends
    def print_i():
      print(i)
      ## III. According to the closure lookup, we print 3
    flist.append(print_i)
  return flist

for f in make_functions():
  # I. We call f here. Using lexical scoping, Python goes back to the definition
  f()
```

The way to fix this is to wrap the value that we want to store in another closure:

```py
def make_printer(i):
  return lambda : print(i)

def make_functions():
  flist = []
  for i in [1,2,3]:
    # II. The closure of make-printer will store i as the number it was in the loop
    flist.append(make_printer(i))
  return flist

for f in make_functions():
  # I. We call f here. Using lexical scoping, Python goes back to the definition
  f()
```

When a function is evaluated, all of its free variables are looked up in the closure.  But as per the definition, though closures are created at runtime, the mapping of the free variables is based only on where the creation of the closure appears in the source code.  This is called **lexical scope**, due to the fact that resolution of variables can be done solely by analyzing the source code without running it.

> Lexical scoping resolves names based on source code, whereas dynamic scoping resolves names based on the program state at runtime.

---


# Type Systems

**Type:** Set of *values* with a *set of behaviours* for those values.

**Type System:** Set of rules in a programming language governing the semantics of its types (e.g. how types are defined, how types can be expressed, how types affect semantics of the language).

**Strongly-Typed Language:** These languages tend to have stricter rules at compile time, and that is where most events will occur.  Thus, every value has a fixed type during the execution of a program.

**Weakly-Typed Language:** These languages have looser typing rules and may produce unpredictable results or may perform implicit type conversion at runtime.  The following is an example of *type coercion* in JavaScript:

```js
"5" + 6
// 56

typeof("5" + 6)
//'string'
```

Strong vs. weak typing is not a binary property, but a spectrum of nuanced rules.  Even in a very strongly-typed language, there is still the question of "when does the program know about and check types?"

**Static Typing:** The type of an expression is determined, before execution, directly from the source code.  Most of these languages require a type signature at the time of a variable's declaration (but this is not a hard rule).

**Dynamic Typing:**  Type-checking is performed at runtime, thus type error is a *runtime error*. 

> **Static type analysis can be used as a compiler optimization.**  If compile-time guarantees the types of all expressions, we can drop the type-checks at runtime.

---

# Streams

Streams are essentially a "lazy" analogue of the list data type.  For example, we know that in Racket, all the elements of a list are eagerly evaluated before the list is constructed.

```rkt
(define items (list 1 2 (/ 1 0))) 
; /: division by zero
```

Yet, this is not always necessary; when we traverse a list we are more concerned with processing one item at a time than with knowing all the other items have been evaluated.  This brings us to the recursive definition of a **stream**:

1. A stream is either empty or
2. A *thunk wrapping a value* combined with a *thunk wrapping another stream*.

```rkt
; infinite stream
(define (repeat n)
  (cons (thunk n) (repeat n)))
```

Notice that thunks are used to delay the evaluation of the elements in the stream until they are called.  We say that **streams decouple the creation of data from the consumption of said data**.

```rkt
; Creates a stream containing the given values
; (make-stream <expr> ...) -> stream
(define-syntax make-stream
    (syntax-rules ()
        [(make-stream <expr>) (s-cons <expr> 's-null)]
        [(make-stream <expr> <other-expr> ...)
        (s-cons <expr> (make-stream <other-expr> ...))]))
```

Since Haskell uses lazy evaluation for all of its function calls, the `:` operator does not evaluate all its elements before constructing the list.  Haskell lists have been streams all along.

```hs
x = [1, 2, error "error"]
-- no error warning
head x
-- 1
```

## Self-Updating Streams

We've established that streams are mainly focused on the consumption of data, rather that its creation.  So we would like to create some sort of "iteration" capability for our Racket stream.  But I thought Racket doesn't support mutation? It does in a way, but this is precisely why a function won't do. If I try to  modify a parameter value, it'll only do so within the function - i.e. if you mutate the stream within the function, it won't mutate your stream globally.

We are going to define a macro called `next!` (the `!` is used to denote mutation) so that we actually get the re-writing behaviour that we need.  This macro does the following:

- If the stream is non-empty, update the stream to `s-rest` and return `s-first`
- If the stream is empty, return the symbol `'DONE`.

```rkt
(define-syntax next!
    (syntax-rules ()
        [(next! <s>)
        (if (s-null? <s>)
            'DONE
            (let* ([temp <s>])
                (begin
                    (set! <s> (s-rest <s>))
                    (s-first temp))))]))
```

```rkt
(define g (make-stream 1 2 (/ 3 0) 4))
(next! g)
; 1
(next! g)
; 2
(next! g)
; ERROR /: division by zero
(next! g)
; 4
(next! g)
; 'DONE
```

---

# Continuation

**Continuation:** A data structure that *represents the computational process at a given point* in the process's execution. 

- For each subexpression `s`, we define its continuation to be a representation of what remains to be evaluated, **only after `s` has been evaluated**. That is, a representation of the control flow from the moment after `s`' evaluation, up to the final evaluation of the enclosing top-level expresssion.
- An expression's continuation is indpendent from the expression itself, but dependent from the expression's position in the larger program.

**First-class Continuations:** Constructs that give a programming language the ability to save the execution state at any point and return to that point later, possibly multiple times.

**Reification:** Process by which an abstract idea about a computer program is turned into an explicit data model created in a programming language. 

Racket *reifies* first-class continuations, meaning that the program can access and manipulate them as easily as it does a list.  It does this trough the `shift` function which follows the following pattern:

```
(shift <id> <body> ..)
```

1. The continuation of the `shift` expression is bound to `<id>`.  We will used `k` as the conventional name for this variable.
2. The `<body>` is evaluated, with `<id>` in scope.
3. The current continuation `k` is discarded, and the value of the last expression in `<body> ...` is returned.

It is important to note that `shift` does not automatically apply its own continuation!  If we want to apply `k`, we need to do so explicitly.

```rkt
(+ 2 (shift k (k 3))) ; k = (+ 2 _)
; 5

(+ 2 (shift k (3))) ; k = (+ 2 _) but is not used
; 3
```

However, `shift` captures the *entire continuation*, beyond what we may want in  most cases. We can place a sort of breakpoint that `shift` is not allowed to go beyond.

```rkt
; computation related to the definition of a will not be included in the continuation
(define a (reset (+ 2 (shift k (* (k 3) (k 4))))))
```

---

# Appendix

## A New Approach to OOP

We said at the beginning that functional programming and OOP were actually equivalent, in that both models had equivalent computational power.  It turns out implementing the concept of an "object" is very straight-foward using the concept of a closure!

If you think about it, the "instace attributes" of an object are free variables that the object must be able to refer to, but that the client can't access without sending a message to the object.  Well closures provide this level of encapsulation too!

```rkt
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
```
```rkt
(define p (Point 1 2))

p
; #<procedure>
(p 'to-string)
; #<procedure>
((p 'to-string))
; "(1, 2)"
```

While this is very cool, Racket macros will allow us to emulate a "class" in a way that we don't have to hard code constructor and this message-handling code.  Because we are interested in deifning new indetifiers for class constructors, we turn to macros to abstract the details of a class.

```rkt
(define-syntax my-class
    (syntax-rules (method)
        [(my-class <class-name> (<att> ...) (method (<method-name> <param> ...) <body>) ...)
        (define (<class-name> <att> ...)
            (lambda (msg)
                (cond   [(equal? msg '<att>) <att>] ...
                        [(equal? msg '<method-name>) (lambda (<param> ...) <body>)] ...
                        [else "Unrecognized message"])))]))
```
```rkt
; create the "constructor"
(my-class Point (x y)
    (method (distance other-point)
        (let* ([dx (- x (other-point 'x))]
              [dy (- y (other-point 'y))])
              (sqrt (+ (* dx dx) (* dy dy))))))

; instantiate two objects
(define p (Point 1 2))
(define q (Point 1 1))
((p 'distance) q)
; 1
```

## The Ambiguous Choice Operator `-<`

The ambiguous operator `-<`  is used to make a non-deterministic choice, and generate a stream of answers.  It takes an arbitrary number of argument subexpressions, and returns a stream of possible values for the whole expression it is found in.

The idea is to provide a number of possible values for an expression in the code.  So we would like the `-<` operator to not only store a stream of choices, but also apply the computational context surrounding it.  For example:

```
(+ 10 (-< 1 2 (+ 3 4))) -> stream (11, 12, 17)
```

Essentially we want to map the continuation `k` across all the values contained by `-<`, and return a stream of the mapped values.

> The `.` operator in Racket is used for functions that take an arbitrary number of arguments.  It simply stores all the arguments passed to a function in a list instead of storing it as a single value.
> ```rkt
> (define (f . x) x)
> (f 3)
> ; '(3)
> ```

```rkt
; Applies k to all elements in the stream
(define (map-stream k lst)
    (if (empty? lst)
        s-null
        (s-cons (k (first lst)) (map-stream k (rest lst)))))

; naive amb operator
(define (-< . options)
    (shift k (map-stream k options)))
```
```rkt
(define g (reset (+ 1 (-< 3 4))))

(next! g)
; 4
(next! g)
; 5
```

### Branching Choices

As soon as we have two `-<` interacting with each other, we lose our desired behaviour because we are trying to apply a stream as the continuation to another stream.  This gives us a stream of streams, rather than just values.

```rkt
(define g (reset (+ (-< 1 2) (-< 3 4))))
(next! g)
; #<procedure>
```

The problem here is that `k` (the function we are mapping over the stream) sometimes returns a single stream and sometimes returns a list of streams.  If we can have `k` always return a list of streams, then we can just `s-append` all the results into a single stream.  **The idea is to change the way we use `-<` so that the continuation `k` always returns a stream.**

1. Assume that `k` always returns a stream of results.
2. Wrap the expression containing the `-<` in a `make-stream` function (this will wrap the entire expression in a stream so that the continuation `k` always maps elements to a stream), then append the streams during the mapping with `s-append-map`.

After we implement these two functions, our `-<` will look like this:

```rkt
(define (-< . options)
    (shift k (s-append-map k options)))
```

```rkt
; STEP 1: Assume k always return a stream
(define (s-append s t)
    (cond [(s-null? s) (t)]
    [(pair? s) (s-cons (s-first s) (s-append (s-rest s) t))]))

(define (s-append-map k lst)
    (if (empty? lst)
        s-null
        (s-append (k (first lst))
            (thunk (s-append-map k (rest lst))))))

; STEP 2: Wrap every expression with -< in a stream
; make-stream will not work because it is a macro, expansion happens 
; before the call to -< is evaluated. we actually need eager evaluation
(define (singleton x) (make-stream x))

; wrapping everything in (reset (singleton (...))) is annoying. write macro
(define-syntax do/-<
    (syntax-rules ()
        [(do/-< <expr>) (reset (singleton <expr>))]))
```


