-- CONSIDER THIS
-- You work for a company that has 10,000 employees, and some of them want to
-- play on an after-work softball team. The company has five teams, named after
-- colors, which you want to use to assign employees:

-- teams = ["red","yellow","orange","blue","purple"]

-- You have a list of employees and you want to match them to the correct team
-- as evenly as possible. What’s a simple way that you can use Haskell’s list
-- functions to perform this task?

-- ------
-- Q 6.1 Haskell has a function called repeat that takes a value and repeats it
-- infinitely. Using the functions you’ve learned so far, implement your own
-- version of repeat.
repeatit x = cycle [x]

-- -----
-- Q 6.2 Write a function subseq that takes three arguments: a start position,
-- an end position, and a list. The function should return the subsequence
-- between the start and end. For example:
-- GHCi> subseq 2 5 [1 .. 10]
-- [3,4,5]
-- GHCi> subseq 2 7 "a puppy"
-- "puppy"
subseq start stop xs = take remaining (drop start xs)
    where remaining = stop-start

-- ------
-- Q 6.3 Write a function inFirstHalf that returns True if an element is in the
-- first half of a list, and otherwise returns False.
