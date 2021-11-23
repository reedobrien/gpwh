import qualified Data.Map as Map -- Figure 18.3
-- CONSIDER THIS
-- Suppose you want to create a type representing a pair of two values of the
-- same type. They could be a pair of Doubles representing latitude and
-- longitude, a pair of Names representing a couple dating, or a pair of graph
-- nodes representing an edge, for example. You don’t want to use the existing
-- Tuple type because you want to ensure that the elements in your pair are
-- exactly the same type. How can you accomplish this?
-- 1. Write type using record syntax
-- 2. Parameterized types

-- Figure 18.1 Definition of the Box parameterized type
data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

-- QUICK CHECK 18.1
-- Q1: What’s the type of wrap (Box 'a')?
-- Box (Box Char)

-- Listing 18.2. Defining the Triple type
data Triple a = Triple a a a deriving Show

-- Listing 18.3. Defining a 3D point in space as a Triple
type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

-- Listing 18.4. Using a Triple to define a name data type
type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"

-- Listing 18.5. Using a Triple to define Initials
type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

-- Listing 18.6. Assessors for the Triple type
first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ x _) = x

third :: Triple a -> a
third (Triple _ _ x) = x

-- Listing 18.7. Defining a toList function on Triple
toList :: Triple a -> [a]
toList (Triple x y z) = [x,y,z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

-- QUICK CHECK 18.2
-- Q1: What’s the difference between transform and the map function for lists? (Hint:
-- Look up the type signature of map again.)
-- 1. map works on any length list, transform only on triples
-- 2. map allows changing the returned type, transform does not

-- Figure 18.2. The definition of a List
-- data [] a = [] | a:[a]

-- Listing 18.9. Defining your own list
data List a = Empty | Cons a (List a) deriving Show

-- Listing 18.10. Comparing your List to the built-in list
builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]

ourListEx1 :: List Int
ourListEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'c':'a':'t':[]

ourListEx2 :: List Char
ourListEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))

-- Listing 18.11. Defining ourMap for your list
ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap func (Cons a rest) = Cons (func a) (ourMap func rest)

-- Listing 18.12. Definition of a tuple
-- data (,) a b = (,) a b

-- Listing 18.13. Exploring the types of tuples
itemCount1 :: (String, Int)
itemCount1 = ("Erasers", 25)

itemCount2 :: (String, Int)
itemCount2 = ("Pencils" , 25)

itemCount3 :: (String, Int)
itemCount3 = ("Pens", 13)

-- Listing 18.14. Creating an item inventory
itemInventory :: [(String, Int)]
itemInventory = [itemCount1, itemCount2, itemCount3]

-- QUICK CHECK 18.3
-- Q1: What would happen if you tried to add ("Paper",12.4) to your inventory?
-- Error because it is a double, and our inventory takes Int.

-- QUICK CHECK 18.4
-- Q1: What’s the kind of (,,)?
-- GHCI Says:
--  :k (,,)
-- (,,) :: * -> * -> * -> *
-- The book says...
-- (a,b,c)
-- Which matches the type info :i
-- :i (,,)
-- type (,,) :: * -> * -> * -> *
-- data (,,) a b c = (,,) a b c

-- Listing 18.15. The Organ data type
data Organ = Heart
        | Brain
        | Kidney
        | Spleen deriving (Eq, Enum, Ord, Show)

-- Listing 18.16. An example list of organs
organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

-- Listing 18.17. A List of IDs to represent the locations of various organs
ids :: [Int]
ids = [2,7,13,14,21,24]

-- Figure 18.4. The fromList function for building a Map
-- fromList :: Ord k => [(k,a)] -> Map k a

-- Listing 18.18. Pairs of organs and IDs v1
-- pairs = [(2, Heart), (7, Heart), (13, Brain)]

-- Listing 18.19. organPairs created using zip
organPairs :: [(Int, Organ)]
organPairs = zip ids organs

-- Listing 18.20. Creating your organCatalog
organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

-- Listing 18.21. The type signature for Map.lookup
-- Map.lookup :: Ord k => k -> Map.Map k a -> Maybe a

-- Q18.1 For the types Triple and Box, implement a function similar to map,
-- tripleMap, and boxMap.
boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)
-- boxMap show (Box 100)
-- Box "100"

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)
-- tripleMap show (Triple 1 2 3)
-- Triple "1" "2" "3"

-- Q18.2 Modify the Organ type so that it can be used as a key. Then build
-- a Map, organ-Inventory, of each organ to its count in the organCatalog.
-- Add Ord, Eq, Enum to Organ definition.
values :: [Organ]
values = map snd (Map.toList organCatalog)

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCounts :: [Int]
organCounts = map countOrgan allOrgans
    where countOrgan = (\o -> (length . filter (== o)) values)

organInventory ::  Map.Map Organ Int
organInventory = Map.fromList (zip allOrgans organCounts)
