-- Listing 6.2
isPalindrome word = word == reverse word


-- Listing 6.3
respond phrase = if '!' `elem` phrase
                 then "wow!"
                 else "uh... okay"

-- Listing 6.4
takeLast n list = reverse (take n (reverse list))

-- Listing 6.5
ones n = take n (cycle [1])

-- Listing 6.5
assignToGroups n xs = zip groups xs
    where groups = cycle [1..n]





