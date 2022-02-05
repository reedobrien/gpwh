import System.IO

-- Listing 24.3. Reading from a file and writing to stdout and another file
-- main :: IO ()
-- main = do
--   helloFile <- openFile "hello.txt" ReadMode
--   firstLine <- hGetLine helloFile
--   putStrLn firstLine
--   secondLine <- hGetLine helloFile
--   goodByFile <- openFile "goodby.txt" WriteMode
--   hPutStrLn goodByFile secondLine
--   hClose helloFile
--   hClose goodByFile
--   print "done"

-- Listing 24.4. Checking whether helloFile is empty before printing the first
-- line
-- main :: IO ()
-- main = do
--   helloFile <- openFile "hello.txt" ReadMode
--   eof <- hIsEOF helloFile
--   firstLine <-
--     if not eof
--       then hGetLine helloFile
--       else return "empty"
--   putStrLn "done"

--QUICK CHECK 24.2
-- Q1: Write the code to check whether the second line is empty before writing
-- it to a file.
main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  eof <- hIsEOF helloFile
  firstLine <-
    if not eof
      then hGetLine helloFile
      else return "empty"
  print firstLine
  eof <- hIsEOF helloFile
  secondLine <-
    if not eof
      then hGetLine helloFile
      else return ""
  print secondLine
