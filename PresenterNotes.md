# Notes on Haskell Presentation

## Topics To Cover

*   How To Haskell
    *   What is Haskell?
        *   Functional programming language inspired by Lambda Calculus and named after Haskell Curry
    *   Install/Use

*   Functional Programming
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
    *   Heavy reliance on higher-order functions
        *   A function that takes at least one function as an argumnent *or* returns another function as a result 
        *   Discussed more later...

*   Attributes Of Haskell
    *   Statically Typed
        *   Types are explicitly declared, enforced, and determined at compile time
    *   Purely Functional
        *   Side effects are minimal and are restricted *only* to Monads!
    *   Type Inference
        *   Data type is *assumed* under circumstances it can be 
    *   Lazy Evaluation
        *   Haskell will only do the work when it *needs* to

*   Basic Syntax
    *   Types
        *   Int, String, Boolean, more!
    *   Functions
        *   Functions can take 0-N parameters
        *   () is for grouping, not for function application!
    *   Pattern Matching
        *   Haskell will try to match the patterns you *expect* with as much specificity you give it. Can match literals, data types, and data types contained within data types. You can get kind of crazy with this...
        *   Fibonacci with pattern matching
    *   List Comprehension And Lazy Lists
    *   Guards!
        *   Fibonacci with guards

*   Higher Order `Fun`ctions In Haskell
    *   map
    *   foldl

*   Making Own Types
    *   "Dude, where's my methods?"
    *   Card Data 
*   Data Structures
    *   Linked List
    *   Binary Tree
*   FizzBuzz



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