-- The "magic" main function!
main :: IO ()
main = do
    putStrLn "Haha! I've broken your impurity Haskell."
    putStrLn "I've started to modify the state of things!"
    putStrLn "I have input! I have output!"
    putStrLn "Don't try to restrict me!"
    putStrLn "How did we do this?"
    putStrLn "By making a monadic function!"
    putStrLn "We mark this function as having a type `IO` to note that Input/Output happens in this function."
    putStrLn "Fun fact: every compiled Haskell program is one large IO action."
    putStrLn "To make our 'purely functional' language 