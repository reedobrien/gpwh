{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.IO

-- Listing 24.5. Sample contents of stats.dat file for your fileCounts.hs
-- program
-- hello.txt chars:  29  words:  5  lines:  2
-- what.txt chars:  30000  words:  2404  lines:  1

-- Lazy version
--
-- getCounts :: String -> (Int, Int, Int)
-- getCounts input = (charCount, wordCount, lineCount)
--   where
--     charCount = length input
--     wordCount = (length . words) input
--     lineCount = (length . lines) input

-- countsText :: (Int, Int, Int) -> String
-- countsText (cc, wc, lc) =
--   unwords
--     [ "chars: ",
--       show cc,
--       "words: ",
--       show wc,
--       "lines: ",
--       show lc
--     ]

-- -- QUICK CHECK 24.3
-- -- Q1: Why is it preferable to use unwords instead of combining your strings
-- -- with ++?
-- -- ++ is specific to lists. unwords has a Text as well as a String/[Char]
-- -- implementation.

-- -- Listing 24.8. Putting your code together into main
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let fileName = head args
--   input <- readFile fileName
--   let summary = (countsText . getCounts) input
--   appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
--   putStrLn summary

-- Non lazy version
--
getCounts :: T.Text -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = T.length input
    wordCount = (length . T.words) input
    lineCount = (length . T.lines) input

countsText :: (Int, Int, Int) -> T.Text
countsText (cc, wc, lc) =
  T.pack
    ( unwords
        [ "chars: ",
          show cc,
          "words: ",
          show wc,
          "lines: ",
          show lc
        ]
    )

-- QUICK CHECK 24.3
-- Q1: Why is it preferable to use unwords instead of combining your strings
-- with ++?
-- ++ is specific to lists. unwords has a Text as well as a String/[Char]
-- implementation.

-- Listing 24.8. Putting your code together into main
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TIO.readFile fileName
  let summary = (countsText . getCounts) input
  TIO.appendFile "stats.dat" (mconcat [T.pack fileName, " ", summary, "\n"])
  TIO.putStrLn summary
