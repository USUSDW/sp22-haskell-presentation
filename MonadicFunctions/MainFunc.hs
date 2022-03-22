-- The "magic" main function!
main :: IO ()
main = do
    putStrLn "Muahaha! I've broken your impurity Haskell."
    putStrLn "I've started to modify the state of things!"
    putStrLn "I have input! I have output!"
    putStrLn "Don't try to restrict me!"
    putStrLn "How did we do this?"
    putStrLn "By making a function that returns a Monad!"
    putStrLn "We use a very special Monad type to do Input/Output (IO) in Haskell."
    putStrLn "This type is an `IO` type"
    putStrLn "Every compiled Haskell program is effectively one large IO action."
    putStrLn "To make our 'purely functional' language useful with modern computing," 
    putStrLn "    we need to do *some* state manipulations."
    putStrLn "Haskell containerizes these into Monads; special function types that"
    putStrLn "    indicate side effects may happen in this function."
    putStrLn "This forces you to be *explicit* when making a change to an external state."