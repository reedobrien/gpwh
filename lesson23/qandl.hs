{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Semigroup

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

-- QUICK CHECK 23.1
-- Q1: Create fourthWord once again, making the String type T.Text.
fourthWord :: T.Text
fourthWord = T.pack thirdWord

main :: IO ()
main = do
    print firstWord
    print secondWord
    print thirdWord
    print fourthWord

-- QUICK CHECK 23.2
-- Q1: There’s a language extension called TemplateHaskell. How would you
-- compile templates.hs to use this extension? How would you add it using
-- a LANGUAGE pragma?
{-# LANGUAGE TemplateHaskell #-}
-- or
-- $ ghc app.hs -XTemplateHaskell

-- Listing 23.5. sampleInput of type Text
sampleInput :: T.Text
sampleInput = "This\nis\nInput"
-- *Main> T.lines sampleInput
-- ["This","is","Input"]

-- Listing 23.6. someText as a sample input for words
someText :: T.Text
someText = "Some\ntext\tfor you"
-- *Main> T.words someText
-- ["Some","text","for","you"]

-- Listing 23.7. Code for splitOn example
breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"
-- T.splitOn breakText exampleText
-- ["This is "," to do"]

-- *Main> T.unlines (T.lines sampleInput )
-- "This\nis\nInput\n"
-- *Main> T.unwords (T.words someText )
-- "Some text for you"

-- *Main> T.intercalate breakText (T.splitOn breakText exampleText )
-- "THis is simple to do"


combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some"," ","text"]
-- "some text"

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"
-- "some text"

myLines :: T.Text -> [T.Text]
myLines txt = T.splitOn "\n" txt

myUnlines :: [T.Text] -> T.Text
myUnlines xs = T.intercalate "\n" xs

-- *Main> myLines sampleInput 
-- ["This","is","Input"]
-- *Main> myUnlines (myLines sampleInput )
-- "This\nis\nInput"

sanskrit :: T.Text
sanskrit = "धर्म

श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।
स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"
