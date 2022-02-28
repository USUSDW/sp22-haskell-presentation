import Card

main :: IO
main = do
    let
      c1 = Card Two Spade
      c2 = Card Ace Diamond
      c3 = Card Three Club
      c4 = Card Queen Heart
    putStrLn (show c1) ++ " has value " ++ (show (getValue c1))
    putStrLn (show c1) ++ " has suit "  ++ (show (getSuit c1))
    putStrLn (show c1) ++ " has a score of " ++ (show (cardScore c1))
    putStrLn "Will compare " ++ (show c1) ++ " and " ++ (show c2)
    putStrLn (show compareCards c1 c2)
    putStrLn "Will compare " ++ (show c3) ++ " and " ++ (show c4)
    putStrLn (show compareCards c3 c4)
