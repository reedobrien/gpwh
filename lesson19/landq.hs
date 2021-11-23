-- CONSIDER THIS
-- Suppose you have a simple Map that contains grocery items and indicates the
-- number of them that you need to purchase:
-- groceries :: Map.Map String Int
-- groceries = Map.fromList [("Milk",1),("Candy bars",10),
-- ("Cheese blocks",2)]
--
-- You accidentally look up MILK instead of Milk. What behavior should you
-- expect from your Map, and how can you handle this type of mistake so that
-- your programs can be sure to run safely even in the presence of missing
-- values in your Map?
-- Return Just value or Nothing. Then the caller can extract the value,
-- determine what to do for a default value, or if the lack of a value is an
-- error.
import Data.List
import qualified Data.Map as Map

data Organ =  Heart
            | Brain
            | Kidney
            | Spleen deriving (Eq, Show)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Listing 19.1. Definition of Maybe
-- data Maybe a = Nothing | Just a
-- redifined a la rust so we don't ocllide with Prelude
data Option a = None | Some a


-- QUICK CHECK 19.1
-- Q1: What’s the type of Nothing in the preceding example?
-- Maybe Organ

-- Listing 19.2. List of possibleDrawers in your organCatalog
possibleDrawers :: [Int]
possibleDrawers = [ 1 .. 50 ]

-- Listing 19.3. Definition of getDrawers
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
    where getContents = \id -> Map.lookup id catalog

-- Listing 19.4. A list of availableOrgans that can contain missing values
availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

-- Listing 19.5. countOrgan function counts instances of an Organ
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter
                                (\x -> x == Just organ)
                                available)

-- Listing 19.6. Definition of isSomething
-- my home rolled attempt with what I've learned so far
-- filter (\x -> x /= Nothing) availableOrgans
-- With the stdlib
-- import Data.Maybe
-- filter isJust availableOrgans
isSomthing :: Maybe Organ -> Bool
isSomthing Nothing = False
isSomthing (Just _) = True

-- Listing 19.7. Using isSomething with filter to clean [Maybe Organ]
justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomthing availableOrgans

--Listing 19.8. Definition of showOrgan
showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

-- Listing 19.9. Using showOrgan with map
organList :: [String]
organList = map showOrgan justTheOrgans

-- Listing 19.20?
cleanList = intercalate ", " organList

-- QUICK CHECK 19.2
-- Q1: Write a function numOrZero that takes a Maybe Int and returns 0 if it’s
-- nothing, and otherwise returns the value.
numOrZero :: Maybe Int -> Int
numOrZero (Just i) = i
numOrZero Nothing = 0
