calc :: [String] -> Int
calc (v1 : "+" : v2 : rest) = read v1 + read v2
calc (v1 : "*" : v2 : rest) = read v1 * read v2

main :: IO ()
main = do
  input <- getContents
  let vals = lines input
  print (calc vals)
