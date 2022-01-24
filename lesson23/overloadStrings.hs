{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

myWord :: T.Text
myWord = "dog"

-- Listing 23.4. Using OverloadedStrings to easily assign Text using a literal
aWord :: T.Text
aWord = "Cheese"


main :: IO ()
main = do
    print myWord
    print aWord -- listing 23.4
