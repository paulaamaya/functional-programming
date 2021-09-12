- [Functions](#functions)
  - [Defining Functions](#defining-functions)
- [Lists](#lists)


We can use the compiler to invoke an active mode by typing in `ghci` in the terminal.

To load a script type in `:l scriptname` and the you can interact with the functions in the terminal, provided `scriptname.hs` is in the same folder from which ghci was invoked.

If you change the `.hs` script, just run `:l scriptname` again or `:r`, which reloads the current script. 

To find out more about how a built-in function works simply type `:t function` into the terminal.  For example,

```
ghci> :t sqrt
sqrt :: Floating a => a -> a
```

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
functionName <param1> <param2> ...= body
```

A common theme in functional programming languages is to define basic functions that are obviosly correct, and then combining them into more complex functions.

```hs
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x = if x > 100 then x else x * 2

processNumber :: (Num a, Ord a) => a -> a
processNumber x = doubleSmallNumber x + 1
```

> **Note:** The `if` statement in Haskell is an expression, which means it must evaluate to a value.  Hence, the `else` is always mandatory; ensuring that if-statements always evaluate to a value.

# Lists

In Haskell, lists are a *homogenous* data structure, i.e. they store elements of the same type. 

Much like in C, strings are syntactic sugar for lists of characters.  Hence, we can use list functions on strings.