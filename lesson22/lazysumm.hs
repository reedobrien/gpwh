-- main :: IO ()
-- main = do
--   userInput <- getContents
--   mapM_ print userInput

-- QUICK CHECK 22.3
-- Q1: Use lazy I/O to write a program that reverses your input and prints it
-- back to you.
-- main :: IO ()
-- main = do
--   input <- getContents
--   mapM_ print (reverse input)

-- The book presetned the following solution
-- let reversed = reverse input
-- mapM_ print reversed

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  input <- getContents
  let numbers = toInts input
  print (sum numbers)
