import Data.List
-- CONSIDER THIS
-- So far, when you’ve combined multiple strings, you’ve used ++. This can get
-- tedious for larger strings:
--
-- "this" ++ " " ++ "is" ++ " " ++ "a" ++ " " ++ "bit" ++ " " ++ "much"

-- Is there a better way to solve this?
x = concat ["this" , " " , "is" , " " , "a" , " " , "bit" , " " , "much" ]

-- Listing 17.1. Examples of using function composition to create functions
myLast :: [a] -> a
myLast = head . reverse
last'  = myLast [1,2,3,4]

myMin :: Ord a => [a] -> a
myMin = head . sort
min'  = myMin [1,2,3,4]

myMax :: Ord a => [a] -> a
myMax = myLast . sort
max' = myMax [4,3,2,1]

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)
tall = myAll even [2,4,6,8]
fall = myAll even [1,2,3,4]

myAny :: (a -> Bool) -> [a] -> Bool
myAny testFunc = (foldr (||) False) . (map testFunc)
tany1 = myAny even [2,4,6,8]
tany2 = myAny even [1,2,3,4]
fany = myAny even [1,3,5,7]

-- Listing 17.2. Semigroup for Integer
instance Semigroup Integer where
    (<>) x y = x + y

-- QUICK CHECK 17.2
-- Q1: Can you use (/) to make Int a Semigroup?
-- No because (/) doesn't always return and Int.

-- Listing 17.3. Defining the Color type
data Color = Red
    | Yellow
    | Blue
    | Green
    | Purple
    | Orange
    | Brown
    | Clear deriving (Show, Eq) -- q17.1

-- Listing 17.4. Implementing Semigroup for Color v1
-- Listing 17.5 Reimplementing Semigroup for color to support associativity.
instance Semigroup Color where
    (<>) Clear any = any -- q17.1
    (<>) any Clear = any -- q17.1
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a,b] = Purple
             | all (`elem` [Blue, Yellow, Green]) [a,b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a,b] = Orange
             | otherwise = Brown


-- Figure 17.1 Using guards in howMuch
howMuch :: Int -> String
howMuch n | n > 10 = "A whole bunch"
          | n > 0  = "no much"
          | otherwise = "we're in debt"
-- | <- a guard

-- QUICK CHECK 17.3
-- Q1: Does your implementation of Semigroup for Integers support associativity?
-- Yes, because addition and multiplication are associcative with integers.

-- Listing 17.6. The rational definition of Monoid
-- class Semigroup a => Monoid a where
--     identity :: a
-- Listing 17.7. The actual definition of Monoid
-- class Monoid a where
--     mempty  :: a
--     mappend :: a -> a -> a
--     mconcat :: [a] -> a

-- QUICK CHECK 17.4
-- Q1: If you implement mappend/<> for Integer as * instead of +, what will
-- your mempty value be?
-- mempty (unit) would be 1

-- mconcat ["does"," this"," make"," sense?"]
-- "does this make sense?"

-- mconcat = foldr mappend mempty

-- Listing 17.8. Type synonyms for Events and Probs
-- OBE q 17.2
-- type Events = [String]
-- type Probs  = [Double]
data Events = Events [String]
data Probs  = Probs [Double]

-- Listing 17.9. PTable data type
data PTable = PTable Events Probs

-- Listing 17.10. createPTable makes a PTable ensuring all probabilities sum to
-- 1
createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
    where totalProbs = sum probs
          normalizedProbs = map (\x -> x/totalProbs) probs

-- Listing 17.11. showPair creates a String for a single event-probability pair
showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

-- Listing 17.12. Making PTable an instance of Show
instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

-- Listing 17.13. The cartCombine function for the Cartesian product of lists
cartCombine :: (a -> b ->c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1      = mconcat repeatedL1
          cycledL2   = cycle l2

-- Listing 17.14. combineEvents and combineProbs
-- OBE -> q 17.2
-- combineEvents :: Events -> Events -> Events
-- combineEvents e1 e2 = cartCombine combiner e1 e2
--     where combiner = (\x y -> mconcat [x,"-",y])

-- combineProbs :: Probs -> Probs -> Probs
-- combineProbs p1 p2 = cartCombine (*) p1 p2

-- Listing 17.15. Making PTable an instance of Semigroup
instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable1 = ptable1
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs  = combineProbs  p1 p2

-- Listing 17.16. Making PTable an instance of Monoid
instance Monoid PTable where
    mempty =  PTable [] []
    mappend = (<>)

-- Listing 17.17. Example PTables coin and spinner
coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

-- Q17.1 Your current implementation of Color doesn’t contain an identity
-- element. Modify the code in this unit so that Color does have an identity
-- element, and then make Color an instance of Monoid.
instance Monoid Color where
    mempty        = Clear
    mappend c1 c2 = c1 <> c2
-- See also changes to instance and data earlier in the file.

-- Q17.2 If your Events and Probs types were data types and not just synonyms,
-- you could make them instances of Semigroup and Monoid, where combineEvents
-- and combineProbs were the <> operator in each case. Refactor these types and
-- make instances of Semigroup and Monoid.

combineEvents :: Events->Events->Events
combineEvents (Events e1) (Events e2) = Events (cartCombine combiner e1 e2)
    where combiner (\x y -> mconcat [x, "-", y]

instance Semigroup Events where
    (<>) = combineEvents

instance Monoid Events where
    mappend = (<>)
    mempty  = Events []

combineProbs :: Probs->Probs-> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup Probs where
    (<>) = combineProbs

instance Monoid Probs where
    mappend = (<>)
    mempty  = Probs []
