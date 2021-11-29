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

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

-- Listing 19.11. The core functions process and report
process :: (Maybe Organ) -> Maybe (Location, Container)
process Nothing = Nothing
process (Just organ) = Just (placeInLocation (organToContainer organ))

--Listing 19.11 and QC 19.3 How would you rewrite report so that it works with
-- Maybe (Location, Container) and handles the case of the missing Organ?
report :: Maybe (Location, Container) -> String
report Nothing = "error, organ or container not found"
report (Just (location, container)) = show container ++ " in the " ++ show location

-- Listing 19.12. Ideal definition of processRequest (won’t compile)
-- processRequest :: Int -> Map.Map Int Organ -> String
-- processRequest id catalog = report (process organ)
--     where organ = Map.lookup id catalog

-- Listing 19.13. processAndReport to handle the Maybe Organ data
-- processAndReport :: (Maybe Organ) -> String
-- processAndReport (Just organ) = report (process organ)
-- processAndReport Nothing = "error, ID not found"

-- Listing 19.14. processRequest with support for Maybe Organ
processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = report (process organ) -- processAndReport organ
    where organ = Map.lookup id catalog




