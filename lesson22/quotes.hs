quotes :: [String]
quotes =
  [ "Power, after all, is the capacity to avoid addressing a counter-narrative. - Rob Wallace",
    "Object oriented programs are offered as alternatives to correct ones… -  Edsger W. Dijkstra",
    "I’ve lived through some terrible things in my life, some of which actually happened. - Mark Twain",
    "One is always a long way from solving a problem until one actually has the answer. - Stephen Hawking",
    "Observe your thoughts, don’t believe them.” - Eckhart Tolle"
  ]

lookupQuote :: [String] -> [String]
lookupQuote [] = []
lookupQuote ("n" : xs) = []
lookupQuote (x : xs) = quote : lookupQuote xs
  where
    quote = quotes !! (read x - 1)

main :: IO ()
main = do
  userInput <- getContents
  mapM_ putStrLn (lookupQuote (lines userInput))
