-- Allow this module to be exported for DemoCard.hs
module Card where

data CardSuit = Club | Diamond | Heart | Spade
  deriving (Show, Eq)

cardSuitToInt :: CardSuit -> Int
cardSuitToInt Club    = 0
cardSuitToInt Diamond = 1
cardSuitToInt Heart   = 2
cardSuitToInt Spade   = 3

data CardValue = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Show, Eq)

cardValueToInt :: CardValue -> Int
cardValueToInt Two   = 0
cardValueToInt Three = 1
cardValueToInt Four  = 2
cardValueToInt Five  = 3
cardValueToInt Six   = 4
cardValueToInt Seven = 5
cardValueToInt Eight = 6
cardValueToInt Nine  = 7
cardValueToInt Ten   = 8 
cardValueToInt Jack  = 9
cardValueToInt Queen = 10 
cardValueToInt King  = 11
cardValueToInt Ace   = 12 -- Ace is greatest!

data Card = Card CardValue CardSuit 
  deriving (Show, Eq)

getValue :: Card -> CardValue
getValue (Card val _) = val

getSuit :: Card -> CardSuit
getSuit (Card _ suit) = suit

cardScore :: Card -> Int                        -- there are 13 cards of each suit
cardScore (Card val suit) = (cardValueToInt val) + 13 * (cardSuitToInt suit)

compareCards :: Card -> Card -> String
compareCards c1 c2
  | cardScore c1 == cardScore c2 = "The two given cards are " ++ (show c1) ++ "s and are equal"
  | cardScore c1  > cardScore c2 = "First " ++ (show c1) ++ "s is greater than second " ++ (show c2)
  | cardScore c1  < cardScore c2 = "First " ++ (show c1) ++ "s is less than second " ++ (show c2)