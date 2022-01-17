-- CONSIDER THIS
-- You want to write a program that will let a user test whether words are
-- palindromes. This is easy for a single word, but how can you let the user
-- supply a continuous list of potential palindromes and keep checking as long
-- as the user has words to check?

-- QUICK CHECK 22.1
-- Q1: Write a main that uses mapM to call getLine three times, and then use
-- mapM_ to print out the values’ input. (Hint: You’ll need to throw away an
-- argument when using mapM with getLine; use (\_ -> ...) to achieve this.)
-- exampleMain :: IO ()
-- exampleMain = do
--   vals <- mapM (\_ -> getLine) [1 .. 3]
--   mapM_ putStrLn vals
--
-- This answer from the book failes to comile ad main-- Couldn't match expected type to 'IO (t1 string)'
-- main :: IO ()
-- main = do
--   vals < mapM (\_ -> getLine)
--   mapM_ putStrLn vals

-- QUICK CHECK 22.2
-- Q1: Write your own version of replicateM, myReplicateM, that uses mapM.
-- (Don’t worry too much about the type signature.)

myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n func = mapM (\_ -> func) [1 .. n]
