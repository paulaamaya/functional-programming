- [Fundamentals](#fundamentals)
- [Functions](#functions)
  - [Defining Functions](#defining-functions)
  - [Bindings](#bindings)
  - [Pattern Matching](#pattern-matching)
    - [Value-Equality Pattern Matching](#value-equality-pattern-matching)
    - [Structural Pattern Matching](#structural-pattern-matching)
    - [Guards](#guards)
  - [Anonymous Functions](#anonymous-functions)
  - [Currying](#currying)
- [Tuples](#tuples)
- [Lists](#lists)
  - [Concatenation](#concatenation)
  - [List Functions](#list-functions)
  - [List Comprehension](#list-comprehension)
    - [Predicate Filtering](#predicate-filtering)
    - [Logistic Filtering](#logistic-filtering)
    - [Multiple Lists and Predicates](#multiple-lists-and-predicates)
    - [Nested Comprehensions](#nested-comprehensions)
- [Type System](#type-system)
  - [Basics](#basics)
  - [Currying & Sectioning](#currying--sectioning)
  - [Algebraic Data Types](#algebraic-data-types)
    - [Defining Types](#defining-types)
    - [Unions](#unions)
- [Polymorphism](#polymorphism)
  - [Generic Polymorphism](#generic-polymorphism)
  - [Ad-Hoc Polymorphism](#ad-hoc-polymorphism)
    - [Higher-Order Typeclasses](#higher-order-typeclasses)


The folllowing notes are based on [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)

We can use the compiler to invoke an active mode by typing in `ghci` in the terminal.

To load a script type in `:l scriptname` and the you can interact with the functions in the terminal, provided `scriptname.hs` is in the same folder from which ghci was invoked.

If you change the `.hs` script, just run `:l scriptname` again or `:r`, which reloads the current script. 

To find out more about how a built-in function works simply type `:t function` into the terminal.  For example,

```
ghci> :t sqrt
sqrt :: Floating a => a -> a
```

---

# Fundamentals

Binding names to a value in Haskell is pretty straightoforward,

```hs
<id> = <expr>
```

We can also define if statements which follow the following syntax:

```hs
if <bool> then <expr> else <expr>
```

> **Note:** The `if` statement in Haskell is an expression, which means it must evaluate to a value.  Hence, the `else` is always mandatory; ensuring that if-statements always evaluate to a value.

Due to the recusive nature of these things, we can nest `if`-statements in the `else` expression of the previous one,

```hs
x = 13

if x `mod` 2 == 0 then "Number is divisible by 2"
else if x `mod` 3 == 0 then "Number is divisible by 3"
else "I'm too lazy to keep checking"
```

This can be refactored even further using [pattern matching](#pattern-matching).

---

# Functions

To call a function, write the function name, a space, and then the parameters separated by spaces.

```hs
-- succ returns the sucessor of an ordered value
succ 8
-- 9
min 100 101
-- 100
```

When parsing, function application has the highest precedence of them all.

```hs
succ 9 + min 10 11 + 1
-- 21
(succ 9) + (min 10 11) + 1
-- 21
```

If a function takes two parameters, we can also call it as an infix function by surrounding it with backticks. 

```hs
-- div performs integer division
div 21 10
-- 2
21 `div` 10
-- 2
```

Since spaces are used for function application in Haskell, a statement such as `foo (foo 3)` would be equivalent to writing `foo(foo(3))` in a language like Java.

## Defining Functions

Functions are defined similar to how they are called, namey using spaces to denote parameters rather than parentheses.

> **Note:** Functions cannot begin with capital letters!

```
<id> <param>* = <body>
```

A common theme in functional programming languages is to define basic functions that are obviosly correct, and then combining them into more complex functions.

```hs
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else x * 2

processNumber :: (Num a, Ord a) => a -> a
processNumber x = doubleSmallNumber x + 1
```

## Bindings

So far, most of the bindings we have created are global bindings, using the `<id> = <expr>` syntax.  But like in most languages, there are variables with local scope, most commonly within function bodies.

```hs
addOne :: Num a => a -> a
addOne x = x + 1

addOne 4
-- 5
x
-- error: Variable not in scope: x
```

Apart from organic local bindings in functions, we can explicitly create local bindings in Haskell using the follwoing syntax:

```hs
let {<id> = <expr>}* in <expr>* 
```

The bidings that you define in the `let` part are accessible to the expression after the `in`.  For example,

```hs
cylinderArea :: Floating a => a -> a -> a
cylinderArea r h =  let sideArea = 2 * pi * r * h
                        topArea = pi * (r ^ 2)
                    in sideArea + 2 * topArea
```

---

## Pattern Matching

Pattern matching is a feature that allows us to **specify conditional behaviour depending on the structure of a value**, with some really concise syntax.

```hs
factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1
factorial n = n * factorial(n - 1)
```

### Value-Equality Pattern Matching

Let's consider an example of a value-based function, and see how we can simplify it with pattern matching.

```hs
-- Conditional statment checking value of param
foo :: (Eq p, Num p) => p -> p
foo x = if x == 5
    then 10
    else if x == 10
        then 15
    else x + 13
```

Rather than giving one generic function signature with a conditional statememnt, we give three (one per `if` branch) pattern-based definitions.

```hs
-- Pattern matching
foo2 :: (Eq p, Num p) => p -> p
foo2 5 = 10
foo2 10 = 15
foo2 x = x + 13
```

### Structural Pattern Matching

We can also use pattern-matching to *decompose a value into subparts and bind each subpart to a new identifier that can be used later*.

The most common application of this is pattern matching to lists, where we can deconstruct the list into subparts that can make our recursion more clear - `[]` matches the empty list and `x:xs` matches a list with at least one element.

```hs
-- Strucuture based conditional
listMax :: (Fractional a, Ord a) => [a] -> a
listMax lst = if null lst then - 1 / 0
            else max (head lst) (listMax (tail lst))

-- Structural pattern matching
listMax2 [] = -1/0
listMax2 (x:xs) = max x (listMax2 xs)
```

### Guards

So far, we can only use patterns to make sure sure a value has some form and to deconstruct it into its structural parts. Haskell **guards allow us to test whether some property of a value is true or not**.  You can think of this as an alternative to `if`-statements that controlled flow based on variable properties rather than the value of the variable.

```hs
-- Expression based if statements
checker :: Integral a => a -> String
checker x = if even x then "Divisible by 2"
            else if x `mod` 3 == 0 then "Divisible by 3"
            else "I'm too lazy to keep checking"

-- Guards
checker2 :: Integral a => a -> String
checker2 x
    | even x = "Divisible by 2"
    | x `mod` 3 == 0 = "Divisible by 3"
    | otherwise = "I'm too lazy to keep checking"
```

## Anonymous Functions

 Anonymous functions in Haskell have the following syntax:

 ```hs
\ <param>* ... -> <body>
 ```

 ## Currying

Currying is a neat feature of Haskell that allows us to create functions on the fly.  This feature allows us use **partially applied function calls**, i.e. call a function is called on "too few" parameters.  This doesn't give an error and in fact *returns a new version of the curried function with one of its paramemters permanently set*.

For instance, say we want to filter a list depending on which elements are larger than 5.  We can use the following short-hand notation:

```hs
bigList :: [Integer] -> [Integer]
bigList xs = filter ((<) 5) (xs)
-- The 5 always gets passed as the first arg of the binary operator <
```

---

# Tuples

Tuples are another data type that allows you to store multiple values as one.  However, there are some significant differences with lists:

- **Tuples have a fixed length** - Use tuples when you know exactly how many entries a piece of data should have.  Tuples are much more rigid because each tuple of length $n$ is its own type; so you can't write a general function to append to all tuples.

```hs
-- This gives a mistake by the compiler
-- A tuple is not the same type as a triple, so list is not homogenous
wrong = [(1,2), (3,4), (1,1,1)]
-- Same with this case
-- A pair of numbers is not the same type as a pair of a string and a number
wrong' = [(1,2), ("centre", 2)]
```

- **Tuples can be heterogeneous** - You can store more than one data type in a tuple.

```hs
people = [("John","Doe",33), ("Jane","Doe",29) ]
```

- The are **no singleton tuples** - You cannot have a tuple with only one element.

`fst` takes a pair and returns its first component.

`snd` takes a pair and returns its second component.

`zip` takes two lists and then zips them together into one list by joining the matching elements into tuples - the first elements go together, the second go together, etc. 

```hs
xs = [1,2,3,4]
ys = ["one","two","three","four"]
zip xs ys
-- [(1,"one"),(2,"two"),(3,"three"),(4,"four")]
```

It's especially useful for when you want to combine two lists in a way or traverse two lists simultaneously. 

If the two list lengths don't match, the longer list simply gets cut off to match the length of the shorter one.

```hs
xs = [1..]
ys = [1,2,3,4]
zip xs ys
-- [(1,1),(2,2),(3,3),(4,4)]
```

---

# Lists

In Haskell, lists are a *homogenous* data structure, i.e. they store elements of the same type. 

Much like in C, strings are syntactic sugar for lists of characters.  Hence, we can use list functions on strings.

```hs
-- Indexing a list
xs = [0,1,2,3]
xs !! 2
```

When using `<`, `<=`, `>` and `>=` to compare lists, they are compared in lexicographical order. First the heads are compared. Only if they are equal does the comparison continue.

```hs
[100,1,2] > [10,20,30]
-- True
[3,4,2] > [2,4]
-- True
[1,2] == [1,2]
-- True
```

We can also use ranges to specify lists, even using some basic step values.

```hs
[1..10]
-- [1,2,3,4,5,6,7,8,9,10]
[2,4..11]
-- [2,4,6,8,10]
[10,9..(-2)]
-- [10,9,8,7,6,5,4,3,2,1,0,-1,-2]
['a'..'k']
-- "abcdefghijk"
```

## Concatenation

In order to concatenate two lists, we use the `++` operator.

```hs
[1,2,3] ++ [4]
-- [1,2,3,4]
['h'] ++ ['i']
-- "hi"
```

Be careful when using the concatenation operator on really long lists since it will be computationally expensive.  Intenally, Haskell has to walk through the entire list on the left before "appending" the new elements.

On the other hand, putting something at the beginning of a list with `:` is automatic.  This is also called `cons` operator.

Since Haskell is a functional programming language, we construct lists using a recursive definition with `cons`,

```hs
l = [3,2,1]
x = 4
y =5

xs = x : l
-- [4,3,2,1]

ys = y : xs
-- [5,4,3,2,1]
```
```hs
4 : [0,0,0]
-- [4,0,0,0]
'A' : " Smelly Cat"
-- "A Smelly Cat"
[1,2,3]
-- Equiv: 1:2:3:[]
```

## List Functions

`head` takes a list and returns its first element.

```hs
head [5,4,3,2,1]  
-- 5
```   
`tail` takes a list and returns all its elements, minus the first. In other words, it chops off a list's head.

```hs
tail [5,4,3,2,1]  
-- [4,3,2,1]  
```

`init` takes a list and returns everything except its last element.

```hs
init [5,4,3,2,1]  
[5,4,3,2]
```

`last` takes a list and returns its last element.

```hs
last [5,4,3,2,1]  
-- 1
```   

`length` takes a list and returns its length.

`null` checks if a list is empty. If it is, it returns True, otherwise it returns False. Use this function instead of `xs == []` 

`reverse` reverses a list.
  
`take` takes number and a list. It **extracts that many elements from the beginning of the list**.

If we try to take more elements than there are in the list, then it just returns the list.

```hs
take 3 [5,4,3,2,1]  
-- [5,4,3]  
take 1 [3,9,3]  
-- [3]  
take 5 [1,2]  
-- [1,2]  
take 0 [6,6,6]  
-- []  
```

`drop` works in a similar way, but it drops the given number of elements from the beginning of a list.

```hs
ghci> drop 3 [8,4,2,1,5,6]  
[1,5,6]  
ghci> drop 0 [1,2,3,4]  
[1,2,3,4]  
ghci> drop 100 [1,2,3,4]  
[]  
```

`maximum` takes a list of stuff that can be put in some kind of order and returns the biggest element. `minimum` returns the smallest.

`sum` takes a list of numbers and returns their sum.

`product` takes a list of numbers and returns their product.

`elem` takes a value and a list then tells us if the given value is an element of the list. It's usually called as an infix function because it's easier to read.

```hs
4 `elem` [3,4,5,6]  
True  
elem 10 [3,4,5,6]  
False
```

`replicate` takes in an integer and an element, then returns a list with the element repeated integer number of times.

```hs
replicate 3 5
-- [5,5,5]
replicate 3 [1,2]
-- [[1,2],[1,2],[1,2]]
```

`cycle` takes a list and cycles it into an infinite list. Since the list goes on forever, to display it you have to slice it off somewhere.

```hs
take 10 (cycle [1,2,3])  
-- [1,2,3,1,2,3,1,2,3,1]  
take 12 (cycle "LOL ")  
-- "LOL LOL LOL " 
```  
`repeat` takes an element and produces an infinite list with only said element. Once again, you have to slice it to display it.

```hs
take 10 (repeat 5)  
[5,5,5,5,5,5,5,5,5,5]
```

Although it's simpler to just use the replicate function if you want some number of the same element in a list. replicate 3 10 returns [10,10,10].

`map` takes in a callback function `f` and a list, then applies `f` to every element in the list producing a new list.

```hs
map (+ 1) [1,2,3,4,5]
-- [2,3,4,5,6]
```

`filter` takes in a predicate `p` and a list, then creates a new list containing all the elements in the original list that evaluated to `True`.

```hs
filter even? [1,2,3,4,5]
-- [2,4]
```
The two functions above are powerful for "iterating" over a list, but they are restricted to returning annother list.

> In functional programming, `fold` (or reduce) is a family of higher order functions that process a data structure in some order and build a return value.

`foldl` takes in a binary function, a starting value (or accumulator), and a list to fold up.  It then proceeds to fold the list from left to right.

- The binary function produces the accumulator.
- The binary function is applied from the starting value to the tail of the list.

```hs
foldl (-) 0 [1,2,3,4,5]
-- -15
```

This is an iterative implementation of Haskell's `foldl` written in Python:

```py
acc = init
for x in lst:
  acc = combine(acc, x)
```

---

## List Comprehension

### Predicate Filtering
List comprehensions in Haskell follow a very similar notation to that of sets in mathematics; including filtering by a predicate.  The predicate follows the variable binding declaration, separated by a comma.

```hs
xs = [x^2 | x <- [1..10]]
-- [1,4,9,16,25,36,49,64,81,100]
ys = [y | y <- [1..30], y `mod` 3 == 0]
-- [3,6,9,12,15,18,21,24,27,30]
```

### Logistic Filtering
For example say we want a list such that every odd number < 5 gets replaced with `"PEW!"` and every  odd number >= 5 gets replaced with `"BANG!"`.  If the number is even, then we throw it out of the list.

```hs
boomBangList :: Integral a => [a] -> [String]
boomBangList xs = [if x < 5 then "PEW!" else "BANG!" | x <- xs, odd x]

boomBangList [x | x <- [1..10]]
-- ["PEW!","PEW!","BANG!","BANG!","BANG!"]
```

### Multiple Lists and Predicates

We can perform list comprehensions that have multiple predicates.  Elements in the resulting list have to meet all predicates.

```hs
[x | x <- [1..20], even x, x `mod` 4 == 0]
-- [4,8,12,16,20]
[x^2 | x <- [1..20], odd x, x ^ 2 < 100]
-- [1,9,25,49,81]
```

We can also draw from multiple lists. A **list comprehension from several lists will return the cross product of said lists**, and join them by the output operation we provide.

```hs
[x*y | x <- [1,3,5], y <- [2,4,6]] 
-- [2,4,6,6,12,18,10,20,30]
a = [x+y | x <- [1,3..10], y <- [2,4..10], x + y < 10]
-- [3,5,7,9,5,7,9,7,9,9]
```

### Nested Comprehensions

If you have lists that contai lists, you can also perform nested list comprehensions!  That is, an inner list comprehension and an outer list compresension at the same time.

Say we want to take out all even numbers from this nested list, but without flattening the list:

```hs
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]

xxs' = [[x | x <- xs, odd x] | xs <- xxs]
-- [[1,3,5,3,1,5],[1,3,5,7,9],[1,1,3,1,3,3]]
```

---

# Type System

## Basics

In Haskell, types are denoted by capitalized names like `Bool` and `Char`.  You can always see the type of any expression with the command `#t`.

```hs
:t Bool
-- True :: Bool
:t "Hello"
-- "Hello" :: [Char]
-- Like C, Haskell stores strings as a list of chars
```

Haskell is statically-typed.  Consider the following example:

```hs
:t (if 3 > 1 then "hi" else "bye")
-- (if 3 > 1 then "hi" else "bye") :: [Char]
```

We can finally understand why Haskell requires that both return branches of an if-statement return the same type.  Notice that the compiler can determine the type of an expression without actually evaluating it.

## Currying & Sectioning

Haskell's type system actually helps us to understand currying more in-depth. Consider the type signature of a binary function like `&&`:

```hs
:t (&&)
-- (&&) :: Bool -> Bool -> Bool
```

The `->` operator in Haskell is right-associative, meaning that `(Bool -> Bool) -> Bool == Bool -> (Bool -> Bool)`.  Thus, **Haskell interprets binary functions as unary higher order functions** that return a function with type `Bool -> Bool`.  In fact, the name *currying* comes directly from the lambda calculus which says that *any multiparameter function can be broken down into a composition of unary functions*.

Because binary functions are primarily meant to be used infix, Haskell provides a special syntax for currying infix operators, called **sectioning**.  This allows us to partially apply an operator by fixing either the first or the second argument.

```hs
f = (True &&) -- Equiv. to (&&) True -> True && x
g = (&& True) -- Not possible with reg. currying -> x && True
```

Here is another example of how we can use what we've learned so far to create a very elegant function definition:

```hs
addToAll1 n lst = map (\x -> x + n) lst
addToAll2 n lst = map (+ n) lst
addToAll3 n = map (+ n)
```

---

## Algebraic Data Types

### Defining Types

We finally get to define our own types in Haskell!  Before that it is worth noting that Haskell will reuse expressions/symbols between its,

- **Expression language** which is used to declare variables and evaluate expressions - for example  `(\x -> x + 1)`.
- **Type language** which is used to represent the types of expressions - for example `Integer -> Integer`.

```hs
-- type definition
data Point = Point Float Float

p = Point 1 2

:t p
-- p is a Point
--p :: Point

:t Point
-- Point is a value constructor that returns a value of that type
-- Point :: Float -> Float -> Point
```

How can we access the `Float` attributes of the point values?  Just like we pattern match on primitive values and lists, **we can also pattern match on value constructors** (`:` has secretly been a value constructor for lists all this time).

```hs
distance :: Point -> Point -> Float 
distance (Point x1 y1) (Point x2 y2) = 
    let dx = abs (x1 - x2)
        dy = abs (y1 - y2)
    in 
        sqrt (dx*dx + dy*dy)
```

### Unions

Haskell also supports union types, that is types made up of other struct-based types.  In their most basic forms, unions are enumerations.

In the example below, we are defining a new type `Shape` which has two constructors:  `Circle` and `Rectangle`.

```hs
data Shape = Circle Point Float
            | Rectangle Point Point
```

Notice that when we pattern-match on unions, each constructor gets its own rule (and we can even nest patterns).

```hs
area :: Shape -> Float 
area (Circle _ r) = pi * r * r
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs ((x1 - x2) * (y1 - y2))
```

These constructor definitions coupled with unions, come together to form **algebraic data types** - i.e. composite types, or types formed by combining other types.  These types usually specify the shape of an element through its *variants*.  For more information, read [this article](https://en.wikipedia.org/wiki/Algebraic_data_type).

---

# Polymorphism

## Generic Polymorphism

The best way to talk about this is to examine lists in Haskell.  It turns out `[]` is actually not a type itself, but a *type constructor*; you need to know what is in your list before you can actually produce a type.  

```hs
data [] a = []
            | (:) a ([] a)
```

**Type variable:** Identifier in a type expression that can be instantiated to any type.

**Type constructor:** A function that takes a type `a` and creates a a new type with the type variable instantiated (e.g. note that these are different types `[] Bool`, `[] String`).

```hs
:t []
-- [] :: [a]
:t (:)
-- (:) :: a -> [a] -> [a]
```

However, all the functions that can be called on a list, behave the same way regardless of what is in the list. This is an example of **generic polymorphism**.  

The function or data type is written generically, so that it can handle `a` values *identically* independent of their type.  You can think about generic functions as operating in the same way on the "outer shape" of the data structure, regardless of the "inner shape" of the data's content.

```hs
:t head
-- head :: [a] -> a
```

> Generic polymorphism allows a function or a data type to be written generically, so that it **can handle values uniformly without depending on their type**.

## Ad-Hoc Polymorphism

**Typeclass:** Set of types plus a set of functions that must be able to operate on these types, although their implementation may be completely different depending on the type.

**Member:**  A type is a member of a typeclass if all the type class' functions have been implemented.

A typeclass is like an interface - if a type is a part of a typeclass, that means that it supports and implements the behavior the typeclass.

```hs
class Show a where
  show :: a -> String
```

For instance, to make our `Point` class a member of the `Show` typeclass, we need to do the following:

```hs
-- make Point a member of typeclass Show
instance Show Point where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"
```

**Typeclass Constraint:** Restricts the type of [type variables](#generic-polymorphism) to members of the typeclass.

Consider the type signature of the `Eq` function:

```hs
:t (==)
-- (==) :: (Eq a) => a -> a -> Bool  
```

Everything before the `=>` symbol is called a class constraint. The equality function takes any two values BUT the type of those two values must be a member of the `Eq`  class (this was the class constraint).

> Ad hoc polymorphism allows a function to behave differently, depending on the type of the argument upon which it is applied.

### Higher-Order Typeclasses

We've used the function `map` to map over lists.  But this is just an implementation of the polymorphic fuction `fmap` which performs *structure-preserving mapping over containers*.

```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b

:t fmap
-- Functor f => (a -> b) -> f a -> f b
```

**Functor:** A typeclass that supports mapping where,

- `(a -> b)` is the function that does the mapping
- `f a` is the functor type constructor applied to `a` (so `f a` is a mappable container)
- `f b` is the functor type constructor applied to `b` (so another mappable container `f b` is returned)


An abstract datatype `f a`, which has the ability for its value(s) to be mapped over, can become an instance of the `Functor` typeclass.

Notice that, this time, the typeclass variable is a type constructor itself! We call `Functor` a **higher-order typeclass** since it takes in a type constructor rather than a type.  That is to say, a new Functor `f b` can be made from `f a` by transforming all of `f a`'s value(s), whilst leaving the structure of `f` itself unmodified.