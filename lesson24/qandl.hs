import System.IO

-- CONSIDER THIS
-- In lesson 22, you saw a way to add up numbers entered in as user input. How
-- can you write the same program that works with a file rather than user input
-- (other than manually piping the file into your program)?

-- QUICK CHECK 24.1
-- Q1: If you want to open a file named stuff.txt to read it, what will the
-- function call look like?
-- fh = openFile "path/to/file" ReadMode

-- QUICK CHECK 24.4
-- Q1: Why doesn’t readFile close the handle?
-- Because closing is an IO action which means it will happen immediately.
-- However the contents of the closed file may not have been evaluated at that time,
-- meaning it will lead to an error when the read is actually attempted.
