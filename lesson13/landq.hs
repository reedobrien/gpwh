-- CONSIDER THIS You’ve written the function inc to increment a value a few
-- times as a sample function. But how can you write an incrementing function
-- that works with the wide range of possible numbers you’ve seen?
-- Frustratingly enough, in unit 1, without specifying types, you could do
-- this. How can you write the type signature of an inc function that works on
-- all numbers?
inc :: Num a => a -> a
inc x = x + 1


{-
QUICK CHECK 13.1

Q1: Find the type of the following: aList = ["cat","dog","mouse"]
A:  aList :: [[Char]]


QUICK CHECK 13.2

Q1: Why isn’t division included in the list of functions needed for a Num?
A: division isn't defined for all cases of Num.
-}


-- Listing 13.2. Using type classes: addThenDouble
addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y)*2

{-
-- Figure 13.1. Structure of a type class definition
class TypeName a where
    f1 :: a -> a
    f2 :: a -> String
    f3 :: a -> -> Bool
-}

-- Listing 13.3. Defining your own type class: Describable
class Describable a where
    describe :: a -> String

{-
-- Listing 13.4. Ord type class requires Eq type class
class Eq a => Ord a where
    compare :: a -> a -> Ordering
     (<) :: a -> a -> Bool
    (<=) :: a -> a -> Bool
     (>) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
     max :: a -> a -> a
     min :: a -> a -> a

-- Listing 13.5. Eq type class generalizes the idea of equality
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

-- Listing 13.6. Bounded type class requires values but no functions
class Bounded a wherej
    minBound :: a
    maxBound :: a

-- Listing 13.7. Show type class definition
class Show a where
    show :: a -> String
-}

-- Listing 13.8. Defining the Icecream type
data IceCream = Chocolate | Vanilla
    deriving (Show, Eq, Ord)

{-
QUICK CHECK 13.3
Q1: See which flavor Haskell thinks is superior by deriving the Ord type class.
Vanilla, assuming that it uses lexgraphic sorting for Ord.
However the actual reason is the data constructor order.

data IceCreamReverse = Vanilla1 | Chocolate1
    deriving (Show, Eq, Ord)

*Main> Vanilla1 < Chocolate1
True
*Main> Chocolate<Vanilla
True
*Main>
-}

{-
Q13.1 If you ran the :info examples, you likely noticed that the type Word has
come up a few times. Without looking at external resources, use :info to
explore Word and the relevant type classes to come up with your own explanation
for the Word type. How is it different from Int?

It has a max bound ~twice an Int. So it looks like an unsigned Int.
-}


{-
Q13.2 One type class we didn’t discuss is Enum. Use :info to look at the
definition of this type class, as well as example members. Now consider Int,
which is an instance of both Enum and Bounded. Given the following definition
of inc:

inc :: Int -> Int
inc x = x + 1

and the succ function required by Enum, what’s the difference between inc and
succ for Int?

inc only works on Ints, succ would work on any type that is Enum-erable.
Also succ will raise exception if it is called on maxBound :: Int.
inc will overflow to -(2^32).

-}
inc' :: Int -> Int
inc' x = x + 1

{-
Q13.3 Write the following function that works just like succ on Bounded types
but can be called an unlimited number of times without error. The function will
work like inc in the preceding example but works on a wider range of types,
including types that aren’t members of Num:

cycleSucc :: (Bounded a, Enum a, ? a) => a -> a
cycleSucc n = ?

Your definition will include functions/values from Bounded, Enum, and the
mystery type class. Make a note of where each of these three (or more)
functions/values comes from.
-}

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound -- (==) here means we need Eq Constraint
              then minBound
              else succ n

-- *Main> cycleSucc '\1114111'
-- '\NUL'


