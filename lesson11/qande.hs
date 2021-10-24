{--
CONSIDER THIS

You need to create a simple function for taking the average of a list of
numbers. The most obvious solution is to take the sum of the list and
divide it by the length of the list:
--}

-- myAverage :: [Int] ->  Float
myAverage aList = (fromIntegral s) /  (fromIntegral l)
    where l = length aList
          s = sum aList

x :: Int
x = 2 -- 32/64 bit limited

y :: Integer
y = 2 -- not 32/64 bit limited

-- listing 11.2 Common types Char, Double, and Bool
letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

-- Listing 11.3. List types
values :: [Int]
values = [1,2,3]

testScores :: [Double]
testScores = [0.99,0.7,0.8]

letters :: [Char]
letters = ['a','b','c']

pet :: [Char]
pet = "cat"

anotherPet :: String
anotherPet = ['d','o','g']

-- Listing 11.4. Tuple types
ageAndHeight :: (Int, Int)
ageAndHeight = (34,74)

firstLastMiddle :: (String, String, [Char])
firstLastMiddle = ("Oscar", "Grouch", "D")

streetAddress :: (Int, String)
streetAddress = (209, "B Street")

-- Listing 11.5. Converting from one type to another with half
half :: Int -> Double
half n = fromIntegral n /2


-- QUICK CHECK 11.1
-- Q1:
-- Haskell has a function named div that does perform integer division (it
-- returns only whole numbers). Write halve, which uses div instead, and
-- include a type signature.
half' :: Int -> Int
half' n = div n 2

-- QUICK CHECK 11.2
-- Q1:
-- Write a function printDouble that takes an Int and returns that value
-- doubled as a string.
printDouble :: Int -> String
printDouble n = show (n * 2)

-- Listing 11.6. Example of reading values from strings: anotherNumber
-- aNum :: Int
-- aNum = show "6"

-- Figure 11.4. Desugaring the multi-argument makeAddress into a sequence
-- of single-argument functions
makeAddressLambda = (\number ->
                     (\street ->
                      (\town -> (number, street, town))))

-- QUICK CHECK 11.3
-- Q1:
-- As each argument is passed to makeAddress, write out the type signature
-- of the returned function.
-- Figure 11.3
makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)
-- Pass in the int and return is
-- String -> String -> (Int, String, String)
-- Pass in "street" and return is
-- String -> (Int, String, String)
-- Pass in town and return is
-- (Int, String, String)

-- Listing 11.7. Type signatures for first-class functions: ifEven
ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n


-- Listing 11.8. simpleInt and [char?]
simpleInt :: Int -> Int
simpleInt n = n

simpleChar :: Char -> Char
simpleChar c = c

-- Listing 11.9. Using type variables: simple
simple :: a -> a
simple a = a

-- Listing 11.10. Multiple type variables: makeTriple
makeTriple :: a -> b -> c -> (a,b,c)
makeTriple a b c = (a,b,c)

-- QUICK CHECK 11.4
-- Q1: The type signature for map is as follows:
-- map :: (a -> b) -> [a] -> [b]
-- Why couldn’t it be this?
-- map :: (a -> a) -> [a] -> [a]?
-- a mapped fn might change the type of the returned value.

-- Q11.1 What is the type signature for filter? How is it different from
-- map?
filter :: (a -> Bool) -> [a] -> [a]

-- Q11.2 In Haskell, both tail and head have an error when called on an empty
-- list. You can write a version of tail that won’t fail but instead return
-- an empty list when called on an empty list. Can you write a version of
-- head that returns an empty list when called on an empty list? To answer
-- this, start by writing out the type signatures of both head and tail.
tail :: [a] -> [a]
tail [] = []
tail (x:xs) = xs

-- can't do head

-- Q11.3 Recall myFoldl from lesson 9.

-- myFoldl f init [] = init
-- myFoldl f init (x:xs) = myFoldl f newInit xs
--   where newInit = f init x
-- What’s the type signature of this function? Note: foldl has a different
-- type signature.
myFoldl :: (a -> b -> a) -> a -> [a] -> a

