import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2,  199.5), (3, 199.4)
        , (4, 198.9), (5,  199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]

file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]

file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
        ,(29, 222.8), (30, 223.8), (31, 221.7)
        ,(32, 222.3), (33, 220.8), (34, 219.4)
        ,(35, 220.1), (36, 220.6)]

-- Listing 20.3. The definition of the TS data type
data TS a = TS [Int] [Maybe a]

-- Listing 20.4. createTS to make an easier interface for creating TS types
createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
    where completeTimes = [ minimum times .. maximum times]
          timeValueMap = Map.fromList (zip times values)
          extendedValues = map (\v -> Map.lookup v timeValueMap)
                                completeTimes

-- Listing 20.5. fileToTS to easily convert your file data into TS types
fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
    where (times, values) = unzip tvPairs

-- Listing 20.6. showTVPair to render time/value pairs readable
showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

-- Listing 20.7. Making TS an instance of Show by using zipWith and showTVPair
instance Show a => Show (TS a) where
    show (TS times values) = mconcat rows
        where rows = zipWith showTVPair times values

-- Listing 20.8. Converting all your data files into TS types
ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

-- Listing 20.9. insertMaybePair, a helper function for inserting (k, Maybe v) pairs
insertMaybePair :: Ord k => Map.Map k v -> (k , Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, (Just value)) = Map.insert key value myMap

-- Listing 20.10. combineTS
combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
    where bothTimes = mconcat [t1, t2]
          completeTimes = [minimum bothTimes .. maximum bothTimes]
          tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
          updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
          combinedValues = map (\v -> Map.lookup v updatedMap)
                               completeTimes

-- Listing 20.11. Making TS an instance of Semigroup
instance Semigroup (TS a) where
    (<>) = combineTS

-- Listing 20.12. Making TS an instance of Monoid
instance Monoid (TS a) where
    mempty = TS [] []
    mappend = (<>)

-- Listing 20.13. tsAll easily created using mconcat
tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

-- Listing 20.14. mean to average a list of most number types
mean :: (Real a) => [a] -> Double
mean xs = total/count
    where total = (realToFrac . sum) xs
          count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) = if all (== Nothing) values
                           then Nothing
                           else Just avg
    where justVals = filter isJust values
          cleanVals = map fromJust justVals
          avg = mean cleanVals

-- Listing 20.15. makeTSCompare and useful type synonyms
type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
    where newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
          newFunc (_, Nothing) (i, val) = (i, val)
          newFunc (i, val) (_, Nothing) = (i, val)
          newFunc (i1, Just val1) (i2, Just val2) =
                if func val1 val2 == val1
                then (i1, Just val1)
                else (i2, Just val2)

-- Listing 20.16. compareTS, a generic means of applying comparison functions to TS
compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) = if all (== Nothing) values
                                   then Nothing
                                   else Just best
    where pairs = zip times values
          best = foldl (makeTSCompare func) (0, Nothing) pairs

-- Listing 20.17. Trivially creating minTS and maxTS using compareTS
minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

-- Listing 20.18. Type signature of diffPair
diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

-- Listing 20.20. diffTS to take the diff of a TS
diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues)
    where shiftValues = tail values
          diffValues = zipWith diffPair shiftValues values

-- Listing 20.21. meanMaybe, which takes the mean of a list of Maybe a values
meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals = if any (== Nothing) vals
                 then Nothing
                 else (Just avg)
    where avg = mean (map fromJust vals)

-- Listing 20.22. movingAvg calculates the moving average of a Maybe a list
movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n = if length nextVals == n
                   then meanMaybe nextVals:movingAvg restVals n
                   else []
    where nextVals = take n vals
          restVals = tail vals

-- Listing 20.23. maTS for calculating the moving average of a TS with centering
movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) n = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
    where ma = movingAvg values n
          nothings = replicate (n `div` 2) Nothing
          smoothedValues = mconcat [nothings, ma, nothings]
