{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Environment
import System.Random

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <-
    foldM
      (\bytes func -> func bytes)
      imageFile
      glitchActions
  let glitchedFileName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedFileName glitched
  print "all done"

-- Listing 25.4. intToChar creates a valid byte from an Int
intToChar :: Int -> Char
intToChar int = toEnum safeInt
  where
    safeInt = int `mod` 255

-- Listing 25.5. intToBC takes an Int and gives you a single-character ByteString
intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

-- Listing 25.6. replaceByte removes a byte and replaces it with a new one
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    newChar = intToBC charVal

-- Listing 25.7. randomReplaceByte applies random numbers to replaceByte
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

-- Listing 25.8. sortSection sorts a section of bytes in your file
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

-- Listing 25.9. Randomizing your sortSection by using an IO action
randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

-- QUICK CHECK 25.4
-- Q1: Create a variable glitchActions outside your main that includes all your
-- actions in a list. Don’t forget to give it the correct type.
glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions =
  [ randomReplaceByte,
    randomSortSection,
    randomReplaceByte,
    randomSortSection
  ]
