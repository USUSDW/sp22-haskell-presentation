myFunc1 :: IO Int
myFunc1 = do
  putStrLn "Hello from func1!"
  return 1

myFunc2 :: IO Int
myFunc2 = do
  putStrLn "Hello from func2!"
  return 2

-- chosen by random diceroll
getRandomNumber :: Int
getRandomNumber = 4

main = do
  let
    roll = getRandomNumber
    func1Result = myFunc1 -- never gets evaluated, because Haskell is Lazy!
    func2Result = myFunc2
  if (roll <= 3)
    then func1Result
    else func2Result

