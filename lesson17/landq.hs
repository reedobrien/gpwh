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
    | Brown deriving (Show, Eq)

-- Listing 17.4. Implementing Semigroup for Color v1
-- Listing 17.5 Reimplementing Semigroup for color to support associativity.
instance Semigroup Color where
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
