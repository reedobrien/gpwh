-- doorPrize :: [Int]
-- doorPrize = [1000,2000,3000]
doorPrize :: [Int]
doorPrize = [1000,2000,3000]

boxPrize :: [Int]
boxPrize = [500,20000]
-- boxPrize :: [Int]
-- boxPrize = [500, 20000]


totalPrize :: [Int] 
totalPrize = (+) <$> doorPrize <*> boxPrize

-- totalPrize' :: Int
-- totalPrize' = (+) doorPrize boxPrize
