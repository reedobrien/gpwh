-- 3.1
simple = (\x -> x)
makeChange = (\owed given ->
               if given - owed > 0
               then given-owed
               else 0 )


-- Q3.2

inc = (\x -> x+1 )
double = (\x -> x*2 )
square = (\x -> x*x )
-- counter x = let x = x + 1
--             in
--              let x = x + 1
--              in
--               x

counter x = (\x -> x+1)
            ((\x -> x+1)
            ((\x -> x) x ))

