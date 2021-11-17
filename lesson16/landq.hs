-- CONSIDER THIS

-- You’re writing code to help manage the breakfast menu at a local diner.
-- Breakfast specials are made up of selections of one or more sides, a meat
-- choice, and the main meal. Here are the data types for these options:

data BreakfastSide = Toast | Biscuit | Homefries | Fruit deriving Show
data BreakfastMeat = Sausage | Bacon | Ham deriving Show
data BreakfastMain = Egg | Pancake | Waffle deriving Show
--
-- You want to create a BreakfastSpecial type representing specific combinations
-- of these items that the customer can choose. Here are the options:

-- Kids’ breakfast—One main and one side
-- Basic breakfast—One main, one meat, and one side
-- The lumberjack!—Two mains, two meats, and three sides
-- How can you create a single type that allows for these, and only these,
-- possible selections from your other breakfast types?
data KidsBreakfast = Kids BreakfastMain BreakfastSide deriving Show
data BasicBreakfast = Basic BreakfastMain BreakfastMeat BreakfastSide
data LumberjackBreakfast = Lumberjack BreakfastMain BreakfastMain
                           BreakfastMeat  BreakfastMeat
                           BreakfastSide  BreakfastSide  BreakfastSide

data BreakfastSpecial = KidsBreakfast | BasicBreakfast | LumberjackBreakfast

-- Listing 16.2. C’s author_name and book structs translated to Haskell
-- data AuthorName = AuthorName String String
-- data Book = AuthorName String String Int

-- Listing 16.3. Using record syntax for Book to show the similarity to a C struct
-- data Book = Book {
--       author :: AuthorName
--     , isbn   :: String
--     , title  :: String
--     , year   :: Int
--     , price  :: Double
-- } deriving Show

-- QUICK CHECK 16.1
-- Q1: Rewrite AuthorName by using record syntax.
data AuthorName = AuthorName {
      firstName :: String
    , lastName  :: String
} deriving Show

-- QUICK CHECK 16.2
-- Q1: Assume you have a Car type. How could you represent a SportsCar as a Car
-- with a Spoiler? (Assume that you have a Spoiler type as well.)
-- data Car = Car {}
-- data Spoiler = Spoiler {}
-- data SportsCar = SportsCar Car Spoiler

-- Listing 16.8. A common sum type: Bool
-- data Bool = False | True

-- Listing 16.9. Using a sum type to model names with and without middle names
type FirstName = String
type LastName = String
type MiddleName = String

-- redefined in 16.3
-- data Name = Name FirstName LastName
--    | NameWithMiddle FirstName MiddleName LastName deriving Show

-- Listing 16.10. A Creator type that’s either an Author or an Artist
data Creator = AuthorCreator Author | ArtistCreator Artist deriving Show

-- Listing 16.11. Defining the Author type by using your existing Name type
data Author = Author Name deriving Show

-- Listing 16.12. An artist can be either a Person or a Band
data Artist = Person Name | Band String deriving Show

-- Listing 16.13. Expanding your Name type to work with H.P. Lovecraft
-- Listing 16.14. Making a Creator type for H.P. Lovecraft
-- Listing 16.15. Easily expanding Name to work with Andrew W.K.
data Name = Name FirstName LastName
        |   NameWithMiddle FirstName MiddleName LastName
        |   TwoInitialsWithLast Char Char LastName
        |   FirstNameWithTwoInits FirstName Char Char
        deriving Show

-- Listing 16.14. Making a Creator type for H.P. Lovecraft
hpLovecraft :: Creator
hpLovecraft = AuthorCreator
                (Author
                    (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

-- Listing 16.16. The Book type using Creator
data Book = Book {
        author    :: Creator
    ,   isbn      :: String
    ,   bookTitle :: String
    ,   year      :: Int
    ,   bookPrice :: Double
} deriving Show

-- Listing 16.17. The VinylRecord type
data VinylRecord = VinylRecord {
        artist      :: Creator
    ,   recordTitle :: String
    ,   recordYear  :: Int
    ,   recordPrice :: Double
} deriving Show

-- Listing 16.18. A StoreItem type is either a Book or a VinylRecord
-- data StoreItem = BookItem Book
--     | RecordItem VinylRecord
--     | ToyItem CollectibleToy
--     | PamphletItem Pamphlet

-- Listing 16.19. Adding a CollectibleToy type
-- data CollectibleToy = CollectibleToy {
--         name     :: String
--     ,   toyDesc  :: String
--     ,   toyPrice :: Double
-- }

-- Listing 16.21. An example of using the StoreItem type with a price function
-- price :: StoreItem -> Double
-- price (BookItem     book)     = bookPrice book
-- price (RecordItem   record)   = recordPrice record
-- price (ToyItem      toy)      = toyPrice toy
-- price (PamphletItem pamphlet) = pamphletPrice pamphlet

-- QUICK CHECK 16.3
-- Q1: Assume that Creator is an instance of Show. Write a madeBy function that
-- has the type StoreItem -> String and does its best to determine who made the
-- StoreItem.
-- madeBy :: StoreItem -> String
-- madeBy (BookItem book)     = show (author book)
-- madeBy (RecordItem record) = show (artist record)
-- madeBy _                   = "unknown"

-- Q16.1
-- To further complicate the items in your store, you eventually keep an
-- inventory of free pamphlets. Pamphlets have a title, a description, and
-- a contact field for the organization that provides the pamphlet. Create the
-- Pamphlet type and add it to StoreItem. Additionally, modify the price so
-- that it works with Pamphlet.

-- data Contact = Contact Name

-- data Pamphlet = Pamphlet {
--         contact       :: Contact
--     -- ,   title         :: String
--     -- ,   pamphletDesc  :: String
--     -- ,   pamphletPrice :: Double
-- }

-- Q16.2
-- Create a Shape type that includes the following shapes: Circle,
-- Square, and Rectangle. Then write a function to compute the perimeter of
-- a Shape as well as its area.
