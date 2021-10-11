import Data.Char
-- CONSIDER THIS
-- Here are two functions, add3ToAll and mul3byAll, which add 3 to each member
-- of a list and multiply 3 by each member of the list, respectively:
--
-- add3ToAll [] = []
-- add3ToAll (x:xs) = (3 + x):add3ToAll xs
--
-- mul3ByAll [] = []
-- mul3ByAll (x:xs) = (3 * x):mul3ByAll xs
--
-- Both functions are easy to write and understand, and they share nearly
-- identical structures. Now imagine a function squareAll, which squares each
-- element in a list. The squareAll function also shares this same basic
-- structure. You can probably think of an endless variety of functions that
-- share this exact same pattern. Can you think of how you’d use first-class
-- functions to rewrite these examples by using a new function that takes in
-- a function as an argument and a list and can be used to define both
-- add3ByAll and mul3ByAll?
add3ToAll [] = []
add3ToAll (x:xs) = (3+x):add3ToAll xs

mul3ByAll [] = []
mul3ByAll (x:xs) = (3*x):mul3ByAll xs

-- listing 9.2
map' f [] = []
map' f (x:xs) = (f x):map' f xs

-- listing 9.3
-- filter
-- filter even [1,2,3,4]
-- [2,4]
filter' pred [] = []
filter' pred (x:xs) = if pred x
                      then x:filter' pred xs
                      else filter' pred xs

-- QC 9.1 Implement remove, which removes elements that pass the test
remove pred [] = []
remove pred (x:xs) = if pred x
                     then remove pred xs
                     else x:remove pred xs

-- QC 9.2 Write the function myProduct, which calculates the product of a list
-- of numbers.
product' (x:xs) = foldl (*) 1 xs

-- listing 9.4
rcons x y = y:x
reverse' xs = foldl rcons [] xs

-- listing 9.5
foldL f iv [] = iv
foldL f iv (x:xs) = foldL f newIV xs
    where newIV = f iv x

-- QC 9.3: True or false: The nongoal step in myFoldl terminates.
-- True if the list is not infinite.

-- listing 9.6
foldr' f iv [] = iv
foldr' f iv (x:xs) = f x right
    where right = foldr' f iv xs


-- Q 9.1 Use filter and length to re-create the elem function.
elem' a [] = False
elem' a (x:xs) = if x == a
                 then True
                 else length (filter (== a) xs) > 0

-- Q9.2 Your isPalindrome function from lesson 6 doesn’t handle sentences with
-- spaces or capitals. Use map and filter to make sure the phrase “A man a plan
-- a canal Panama” is recognized as a palindrome.
isPalindrome [] = False
isPalindrome s = fixed == reverse fixed
    where lowered = map toLower s
          fixed = filter (/= ' ') lowered

-- Q9.3 In mathematics, the harmonic series is the sum of 1/1 + 1/2 + 1/3 + 1/4
-- .... Write a function harmonic that takes an argument n and calculates the
-- sum of the series to n. Make sure to use lazy evaluation.
harmonic n = sum (take n values)
    where pairs  = zip (cycle [1.0]) [1.0,2.0..]
          values = map (\pair -> (fst pair)/(snd pair)) pairs
