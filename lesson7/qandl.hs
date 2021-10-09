-- Consider this:
-- In the preceding lesson, you learned the function take, which allows you to
-- take n elements from a list:
myTake n _ -- less than 1 == []
    | n<= 0 = []
myTake _ [] = [] -- empty list == empty list
myTake n (x:xs) = x : myTake (n-1) xs -- recursive take using pattern matching

-- listing 7.1
myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
    where remainder = mod a b


-- listing 7.2 
sayAmount n = case n of
    1 -> "one"
    2 -> "two"
    n -> "a bunch"

-- listing 7.3
sayAmount' 1 = "one"
sayAmount' 2 = "two"
sayAmount' n = "a bunch"

-- QC 7.3
-- Fill in this definition of myTail by using pattern matching, and make sure
-- to use _ where the value isn’t needed:
-- myTail (<fill in this>) = xs
myTail (_:xs) = xs

-- Q 7.1
-- The tail function in Haskell returns an error when called on an empty list.
-- Modify myTail so that it does handle the case of an empty list by returning the
-- empty list.
myTail (_:xs) = xs
myTail [] = error "tail called on an empty list"

-- Q 7.2
-- Rewrite myGCD by using pattern matching.
myGCD' a 0 = a
myGCD' a b = myGCD b $ mod a b

