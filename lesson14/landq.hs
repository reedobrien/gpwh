-- CONSIDER THIS

-- You have a data type consisting of data constructors for New England states:
data NewEngland = ME | VT | NH | MA | RI | CT

-- You want to be able to display them by their full name by using Show. You can
-- easily display their abbreviations by deriving show, but there’s no obvious way
-- to create your own version of show. How can you make your NewEngland type
-- display the full state name by using show?
instance Show NewEngland where
    show ME = "Maine"
    show VT = "Vermont"
    show NH = "New Hampshire"
    show MA = "Massachusettes"
    show RI = "Rhode Island"
    show CT = "Connecticut"

-- Listing 14.1. Defining the SixSidedDie data type
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

-- Listing 14.2. The SixSidedDie type deriving Show
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6  deriving (Show)

-- Listing 14.3. Creating an instance of Show for SixSidedDie
-- instance Show SixSidedDie where
--     show S1 = "one"
--     show S2 = "two"
--     show S3 = "three"
--     show S4 = "four"
--     show S5 = "five"
--     show S6 = "six"

-- QUICK CHECK 14.1
-- Q1: Rewrite this definition of show to print the numerals 1–6 instead.
data RomanSixSidedDie = R1 | R2 | R3 | R4 | R5 | R6
instance Show RomanSixSidedDie where
    show R1 = "I"
    show R2 = "II"
    show R3 = "III"
    show R4 = "IV"
    show R5 = "V"
    show R6 = "VI"

-- Listing 14.4. Incorrect attempt to implement show for SixSidedDie
-- show :: SixSidedDie -> String
-- show S1 = "one"
-- show S2 = "two"
-- show S3 = "three"
-- show S4 = "four"
-- show S5 = "five"
-- show S6 = "six"

-- Listing 14.5. Demonstrating the need for polymorphism defining :show for TwoSidedDie
data TwoSidedDie = One | Two

show :: TwoSidedDie -> String
show One = "one"
show Two = "two"


-- Listing 14.6. Implementing an instance of Eq for SixSidedDie
-- instance Eq SixSidedDie where
--     (==) S6 S6 = True
--     (==) S5 S5 = True
--     (==) S4 S4 = True
--     (==) S3 S3 = True
--     (==) S2 S2 = True
--     (==) S1 S1 = True
--     (==) _  _  = False

-- QUICK CHECK 14.2
-- Q1: Use Hoogle to search for the RealFrac type class. What’s its minimal
-- complete definition?
-- Go to https://hoogle.haskell.org and search
-- Click
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#t:RealFrac
-- in the results
-- Minimal complete definition
-- properFraction <==

-- Listing 14.8. How deriving Ord is determined
data Test1 = AA | ZZ deriving (Eq, Ord)
data Test2 = ZZZ | AAA deriving (Eq, Ord)

-- 14.5. Implementing Ord
-- instance Ord SixSidedDie where
--     compare S6 S6 = EQ
--     compare S6 _  = GT
--     compare _  S6 = LT
--     compare S5 S5 = EQ
--     compare S5 _  = GT
--     compare _  S5 = LT
--     compare S4 S4 = EQ
--     compare _  S4 = LT


-- QUICK CHECK 14.4
-- Q1: Rewrite SixSidedDie to derive both Eq and Ord.
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum, Eq, Ord)
instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

-- Listing 14.9. Implementing Enum for SixSidedDie (errors with implementation)
-- instance Enum SixSidedDie where
--     toEnum 0 = S1
--     toEnum 1 = S2
--     toEnum 2 = S3
--     toEnum 3 = S4
--     toEnum 4 = S5
--     toEnum 5 = S6
--     toEnum _ = error "No such value"

--     fromEnum S1 = 0
--     fromEnum S2 = 1
--     fromEnum S3 = 2
--     fromEnum S4 = 3
--     fromEnum S5 = 4
--     fromEnum S6 = 5

-- Listing 14.10. Using a type synonym for Name
-- type Name = (String, String)

names :: [Name]
names = [ (Name ("Emil", "Cioran"))
        , (Name ("Eugene", "Thacker"))
        , (Name ("Friederich", "Nietzsche"))]

-- Listing 14.11. Attempt to implement Ord for a type synonym
instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

-- Listing 14.12. Defining a new type Name using data
-- data Name = Name (String, String) deriving (Show, Eq)
newtype Name = Name (String, String) deriving (Show, Eq) -- preferred

