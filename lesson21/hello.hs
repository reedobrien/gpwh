-- Listing 21.1. A simple Hello World program
helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let statement = helloPerson name
    putStrLn statement

-- QUICK CHECK 21.1
-- Q1: Which line retrieves the user’s input? What type do you assume that input is?
-- A1: name<-getLine

-- CONSIDER THIS
-- You can get a line of user input by using the getLine function. But each time
-- getLine is called, it can clearly return a different result. How can this work,
-- given one of the most important features of Haskell is always returning the
-- same value for the same input?

-- QUICK CHECK 21.2
-- Q1: Is it okay if the last line in your main is getLine?
-- No because the signature is `IO String`, but the main return is `IO ()`

-- QUICK CHECK 21.3
-- Q1: Could you simplify your code to combine helloPerson and getLine like this?
-- let statement = helloPerson getLine
-- No helloPerson takes a String and getLine still returns an IO String.
-- There is no `<-` here to take the IO String out of IO in the do notation.
