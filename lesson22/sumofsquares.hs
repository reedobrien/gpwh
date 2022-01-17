-- QUICK CHECK 22.4
-- Q1: Write a program that returns the sum of the squares of the input.

main :: IO ()
main = do
  input <- getContents
  let numbers = toInts input
  let squares = map (^ 2) numbers
  print (sum squares)

toInts :: String -> [Int]
toInts = map read . lines

square :: [Int] -> [Int]
square = map (^ 2)
