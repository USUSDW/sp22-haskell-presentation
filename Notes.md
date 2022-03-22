# Notes on Haskell Presentation

**NOTE: Not *all* of the demonstrations have been fully documented in these notes. Some sections might only have bullet points for the time being. These sections will be finished soon.**

## What Even *Is* Haskell?
Haskell is a general-purpose *purely functional* programming language named after Haskell Curry. This language first appeared in 1990, and has shaped the fields of Mathematics and Computer Science ever since. Haskell has directly influenced many of our modern programming languages. Haskell particularly shines when tasked with solving a problem that is *well defined*. Algorithms written in Haskell can be mathematically proven to be correct given it's lack of side effects, (general) non-reliance on state, and guarantee that the output is always reproducable when given select input. Some programs written in Haskell are XMonad, a dynamic window manager for the X Window System, Pandoc, a tool to convert from one Markup language to another, and Facebook's spam filtering algorithms.

## How To Use Haskell

### Local Tools
The following tools are utilized when compiling and building Haskell projects locally.

#### GHC

Glasgow Haskell Compiler, the most commonly used Haskell compiler, is written in Haskell. It produces some of the fastest compiled Haskell code of all the compilers out there. GHC also has an *interactive REPL*, accessible through `ghci`. Usually, `ghc` is *one of* the tools needed to use Haskell, but it is the most important.

*   **NOTE:** If `ghc` is installed directly and not with Stack, you *may* need to add the `-dynamic` option to `ghc` when compiling on select systems. This is needed to resolve `Could not find module 'Prelude'` error.

*   [Install GHC (NOT RECOMMENDED, USE STACK LISTED BELOW!)](https://www.haskell.org/ghc/)

#### Stack

Stack is a build tool for Haskell code that unifies the work flow of various Haskell tools. It gives you tools to compile (`ghc`) Haskell files, build large projects (`cabal`), and provides easy access to packages on the Hackage package repository; as well as providing a curated subset of "stackage" packages tested for compatability and aimed to promote stability with Haskell packages and avoid dependency issues. If you will be doing any actual project development with Haskell, utilizng Stack is recommended.

An installation of Stack will provide you with *all* necessary tools to compile and build Haskell programs. Installing `stack` will also install a version of `ghc` and `ghci` linked with `stack`. 

*   [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

### REPL.it

You can utilize the REPL.it free online Haskell REPL if you would like to follow along with this presentation. In the provided shell, you have immediate access to the `ghci` command to access the interactive Glasgow Haskell Compiler REPL, as well as `ghc` to compile the `main.hs` file you will be editing on the left. Please do note that `stack` *is* installed in this shell, however `stack ghc` and `stack ghci` produce errors. It is recommended to just use `ghc` and `ghci` and avoid `stack` on REPL.it. 

*   [Haskell on REPL.it](https://repl.it/languages/haskell)

### Key Shell Commands To Know For Haskell
*   `ghc [FILE]`
    *   Invoke Glasgow Haskell Compiler to compile `[FILE]`
*   `ghci`
    *   Invoke Glasgow Haskell Compiler in interactive mode and get into the REPL
    *   `ghci [FILE]` will load the file and put you into interactive mode
*   `stack`
    *   Interface with the `stack` build tool for Haskell
    *   If `ghc` is installed through `stack` only, it (and `ghci`) can be accessed through `stack ghc`

## Language Attributes Of Haskell

*   Attributes Of Haskell
    *   Statically Typed
        *   Types are explicitly declared, enforced, and determined at compile time
    *   Purely Functional
        *   Side effects are minimal and are restricted *only* to Monads!
    *   Type Inference
        *   Data type is *assumed* under circumstances it can be 
    *   Lazy Evaluation
        *   Haskell will only do the work when it *needs* to

## Haskell is "functional"... What does functional programming *mean*?
It means it works. __*Badum-tss...*__

On a real note, these are the main attributes of functional programming:

*   Functional programming is declarative
    *   State what you want to happen, the computer figures out the rest
    *   You are able to state the *form* of things rather than *how* to form them
*   Construct programs through function application and composition
*   No/limited side-effects in purely functional programming
    *   Data isn't really modified, only copied
*   Reproducability! A given set of input should produce the same output *every time*
*   Functions are "first class citizens"
    *   Functions can be passed as arguments, returned or stored in data structures
*   Anonymous functions are typically used
    *   Commonly referred to as "lambda functions"
    *   In Haskell, this takes the form `(\x -> x + 1)`, which is an anonymous function taking single parameter `x` and returns `x + 1`
    *   In Python, a lambda function can be created with `(lambda x: x + 1)`, which *also* takes single parameter `x` and returns `x + 1`
*   Heavy reliance on higher-order functions
    *   A function that takes at least one function as an argumnent *or* returns another function as a result
    *   `map`, `fold*`, and `compose` functions are common examples of higher-order functions
        *   We will demonstrate these momentarily!

## Haskell Basics

### Functions
*   Functions can take 0-N parameters
*   () is for grouping, not for function application!
*   For the most part, we primarily use functions to accomplish just about everything we do in Haskell!
*   Functions are *generally* pure in Haskell, however there exists ways to note a function as a "special function" that may be slightly less than pure.
    *   We will demonstrate impure functions later!

### Types
*   Int, String, Boolean, more!
*   Types are primarily used to define functions and *restrict* the data they work on
*   We can define a function that accepts only certain data types and returns select data types

### Pattern Matching
*   Haskell will try to match the patterns you *expect* with as much specificity you give it. Can match literals, data types, and data types contained within data types. You can get kind of crazy with this...
*   Fibonacci with pattern matching

### Guards!
When a pattern is matched, there may exist *other* conditions upon the matched data. If we are wanting to modify the return values of our function based on some *boolean expression*, a guard is perfect. Let us observe a `fib` function utilizing guards. Our fibonacci function will take an integer and return an integer (`Int -> Int`). We will match `fib n`, and based on the result of different boolean expressions based on the value of `n`, we will choose what `fib` should return. **Note:** The `otherwise` keyword in Haskell is shorthand for `True` and aims to improve readability of guard statements.

```haskell
fib :: Int -> Int
fib n
    | n <= 1    = 1
    | otherwise = n * (fib (n - 1)) 
```

The function above basically reads "If n is less than or equal to 1, `fib n` is `1`. Otherwise, `fib n` is `n * (fib (n -1))`." 

### "Impure" Functions, Monads, and More!
We've had a lot of discussion about Haskell's "purity." Some of you may have started to see some limitations that may arise when our functions are *incapable* of modifying state. "How do I create something useful in this language if I can't *really* modify things or produce output?" The creators of Haskell identified this issue too, and came up with a solution! Their solution was to create Monads.

Monads are actually an *incredibly* deep and complex topic. They were created in the 1960s by mathematicians in the [field of category theory](https://en.wikipedia.org/wiki/Monad_(category_theory)), and were rediscovered by computer scientists in the 1990s in an effort to solve the problem with "purely functional" programming as discussed earlier. For our purposes, we can consider a Monad to be a *special wrapper to a function that allows you to contain and control it's impurities and the fact that said function might modify or rely on external state.* We have created a way to explicitly note when a function is pure or impure, and our programming language can correctly handle both cases. Inside of a monadic function, we can call other monadic functions or other pure functions. Inside a pure function, we *cannot* call a monadic function. "Adding purity to impurity is fine, the end result is still impure. Adding impurity to purity makes a mess, and now we've created something impure."

Monads are *far* more complicated and have far more nuance to them than we can get into during this presentation. For now though, we will work with the `IO` monad, and will use it to define our `main` function and any other functions that may produce Input/Output side effects. When we define an `IO` monad, we must dictate it's return type. `IO ()` is an `IO` monad with *nothing* returned, and `IO Int` denotes a monad that should return an `Int`.

For a Haskell program to be compiled and executable, one must define the `main` function to be an `IO ()` monad. During compile time, `ghc` will seek out this `main :: IO ()` function and will treat it as the main entry point into the program. Much like we have a `main` function in Java and C/C++ that are treated as the main entry point when the code is compiled. 

#### FizzBuzz Example
See [FizzBuzz](./FizzBuzz/) to demonstrate a simple FizzBuzz program that prints output to the console. Demonstrates `main` and other "impure" Haskell functions that are used to create a usable program. We define the monadic function `main :: IO()`, the monadic function `doFizzBuzz :: Int -> IO()`, and the pure function `getFizzBuzz :: Int -> String` to accomplish this.

#### Concatenate Example


### List Comrehension And "Lazy Lists"
See [InfiniteLists](./IniniteLists) demo.

## Higher Order `Fun`ctions In Haskell
A higher order function is...

*   map
*   foldl
*   composition (with `.` operator)

See [HigherOrderFuncs](./HigherOrderFuncs/)

## Defining Own Data And Making Your Own Types
*   Card Example!
See [CardData](./CardData/) 

## Data Structure Examples In Haskell
*   Linked List
    *   See [LinkedList](./LinkedList/)
*   Binary Tree
    *   See [BST](./BST)

## Example Programs
*   FizzBuzz


