{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
import System.Random

-- CONSIDER THIS
-- You have the name of the Japanese author Tatsuhiko Takimoto represented in
-- Japanese Kanji using T.Text:

-- tatsuhikoTakimoto :: T.Text
-- tatsuhikoTakimoto = ""

-- You need to know the number of bytes in this text. For ASCII text, this
-- would be the length of the text, but in this case, using T.length gives you
-- only the number of characters (5). How can you find the number of bytes?

-- Listing 25.1 . ByteString defined by using the OverloadedStrings extension
sampleBytes :: B.ByteString
sampleBytes = "Hello!"

-- Listing 25.2. Trying to unpack a ByteString into a String causes an error
samplesString :: String
-- samplesString = B.unpack samplesString  -- Error because this returns Word.Word8
samplesString = BC.unpack sampleBytes

-- QUICK CHECK 25.1
-- Q1: Write a function that takes numbers in ASCII character form and converts
-- them to Ints. For example, make the following an Int:
bcInt :: BC.ByteString
bcInt = "6"

bcToInt :: BC.ByteString -> Int
bcToInt = read . BC.unpack

-- QUICK CHECK 25.3
-- Q1: Write an IO action that returns a random Char.
randomChar :: IO Char
randomChar = do
  randomInt <- randomRIO (0, 255) -- could also use max and min bound
  return (toEnum randomInt)

-- Listing 25.13 Creating a Unicode BC.ByteString
nagarjunaBC :: BC.ByteString
nagarjunaBC = "नागर्जुनॅ"

-- Listing 25.15. Attempting to transform Text into a ByteString
nagarjunaText :: T.Text
nagarjunaText = "नागर्जुन"

-- Listing 25.15 Attempting to transform Text into a ByteString
nagarjunaB :: B.ByteString
nagarjunaB = (BC.pack . T.unpack) nagarjunaText

-- Listing 25.16 Converting between Text and ByteString with de/encodeUtf8
nagarjunaSafe :: B.ByteString
nagarjunaSafe = E.encodeUtf8 nagarjunaText
