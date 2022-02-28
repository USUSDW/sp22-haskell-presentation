getFizzBuzz :: Int -> String
getFizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3  == 0 = "Fizz"
  | n `mod` 5  == 0 = "Buzz"
  | otherwise       = show n

doFizzBuzz :: Int -> IO ()
doFizzBuzz 0 = return () -- do nothing...
doFizzBuzz n = do
  doFizzBuzz (n - 1)
  putStrLn (getFizzBuzz n)

main :: IO ()
main = do
  doFizzBuzz 100