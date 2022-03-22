import System.IO
import System.Environment


catPrint :: Handle -> IO()
catPrint file = do
  end <- hIsEOF file
  if end then
    return ()
  else do 
    line <- hGetLine file
    putStr $ line ++ "\n"
    catPrint file
  
cat :: String -> IO()
cat fName = do
  f <- openFile fName ReadMode
  catPrint f
  hClose f

callCat :: [String] -> IO()
callCat [] = return ()
callCat (x:xs) = cat x >> callCat xs

main :: IO()
main = do
  args <- getArgs
  callCat args
