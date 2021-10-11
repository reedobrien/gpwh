-- Consider This
-- In the preceding lesson, you were asked to consider writing a take function
-- on your own. This time, consider the drop function:
-- drop 3 [1,2,3,4]
-- [4]
drop' _ [] = []
drop' n xs@(_:xs')
    | n < 0 = error "negative drop' value"
    | n > 0 = drop' (n-1) xs'
    | otherwise = xs


--  listing 8.1
length' [] =0
-- length' xs = 1 + length' (tail xs)
length' (x:xs) = 1 + length' xs

-- listing 8.2 Cf. lesson7 Consider this...
myTake 0 _ = []
myTake _ [] = [] -- empty list == empty list
myTake n (x:xs) = x : myTake (n-1) xs -- recursive take using pattern matching
-- myTake n (x:xs) = x:rest
--  where rest = myTake (n-1) xs


-- listing 8.3
-- finiteCycle (first:rest) = first:rest ++ first -- only returns list with fst appended
cycle' (x:xs) = x:cycle' (xs++[x])

-- 8.3.1 The Ackermann function
-- The Ackermann function takes two arguments, m and n. When referring to the
-- mathematical definition of the function, you’ll use A(m, n) to save space.
-- The Ackermann function follows these three rules:
ack 0 n = n+1
ack m 0 = ack (m-1)  1
ack m n = ack (m-1) (ack m (n-1))

-- listing 8.3.2 The Collatz conjecture
-- The Collatz conjecture is an addictively fascinating problem in mathematics.
-- The Collatz conjecture involves defining a recursive process given
-- a starting number n:
-- If n is 1, you’re finished.
-- If n is even, repeat with n/2.
-- If n is odd, repeat with n × 3 + 1.
coll 1 = 1
coll n = if even n
         then 1 + coll (div n 2)
         else 1 + coll (n * 3+1)

-- Q 8.1
-- Implement your own version of reverse, which reverses a list.
reverse' [] = []
reverse' (x:[]) = [x]
reverse' (x:xs) = reverse' xs ++ [x]


-- Q 8.2
-- Calculating Fibonacci numbers is perhaps the single most common example of
-- a recursive function. The most straightforward definition is as follows:
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n-1) + fib (n-2)
--
--Like the Ackermann function, this implementation quickly explodes due to the
--mutually recursive calls. But unlike the Ackermann function, there’s a much
--more efficient way to compute the nth Fibonacci number. Write a function,
--fastFib, that can compute the 1,000th Fibonacci number nearly instantly.
--Hint: fastFib takes three arguments: n1, n2, and counter. To calculate the
--1,000th Fibonacci number, you call fastFib 1 1 1000 and for the 5th, you call
--fastFib 1 1 5.
fastfib' _ _ 0 = 0
fastfib' _ _ 1 = 1
fastfib' _ _ 2 = 2
fastfib' x y 3 = x + y
fastfib' x y c = fastfib' (x + y) x (c -1)

fib n = fastfib' 1 1 n

