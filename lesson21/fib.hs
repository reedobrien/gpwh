fastfib :: Int -> Int -> Int -> Int
fastfib _ _ 0 = 0
fastfib _ _ 1 = 1
fastfib _ _ 2 = 2
fastfib x y 3 = x + y
fastfib x y c = fastfib (x + y) x (c -1)

fib :: Int -> Int
fib n = fastfib 1 1 n

main :: IO ()
main = do
    putStrLn "Input a number n to generate nth fibonacci numbers."
    n <- getLine
    let nth = fib (read n)
    putStrLn (show nth)
