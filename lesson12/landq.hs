{-
CONSIDER THIS
You want to write a function that operates on music albums. An album
includes the following properties (and types): artist (String), album
title (String), year released (Int) and a track listing ([String]). The
only way you know how to store all this data right now is with a tuple.
Unfortunately, this is a bit unwieldy and makes getting information out of
the tuple tedious (because it requires pattern matching each attribute).
Is there a better way to do this?

A: Use data type record syntax, or whatever correct class/type equivalent is in haskell.
-}

-- Listing 12.1. Defining the patientInfo function
-- patientInfo :: String -> String -> Int -> Int -> String
patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- GHCi> patientInfo "John" "Doe" 43 74
-- "Doe, John (43yrs. 74in.)"
-- GHCi> patientInfo "Jane" "Smith" 25 62
-- "Smith, Jane (25yrs. 62in.)"

-- Listing 12.2. Type synonyms: FirstName, LastName, Age, and Height
type FirstName = String
type LastName = String
type Age = Int
type Height = Int

-- Listing 12.3. Type synonym: PatientName
type PatientName = (String, String)

-- Listing 12.4. Accessing PatientName values: firstName and lastName
firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

{-
-- QUICK CHECK 12.1
Q1:  Rewrite patientInfo to use your patientName type, reducing the total arguments
needed to three instead of four.
-}
patientInfoV2 :: PatientName -> Age -> Height -> String
patientInfoV2 (fname, lname) age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- Figure 12.1. Defining the Sex type
data Sex = Male | Female

-- Listing 12.5. Defining the sexInitial function
sexInitial :: Sex -> Char
sexInitial Female = 'F'
sexInitial Male = 'M'

-- Listing 12.6. Defining the type RhType
data RhType = Pos | Neg

-- Listing 12.7. Defining the type ABOType
data ABOType = A | B | AB |O

-- Figure 12.2. Combining ABOType and RhType to create BloodType
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

-- Listing 12.8. Displaying your types: showRh, showABO, showBloodType
showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

-- Listing 12.9. Defining the canDonateTo function
canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

-- Listing 12.10. Support different names: MiddleName and Name
type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

-- Listing 12.11. Displaying multiple constructors: showName
showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

-- Listing 12.12. PatientV1 v.1
data PatientV1 = PatientV1 Name Sex Int Int Int BloodType

johnDoe :: PatientV1
johnDoe = PatientV1 (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

-- QUICK CHECK 12.2
-- Q1: Create a Jane Elizabeth Smith patient by using whatever reasonable
-- values you like.
janeDoe :: PatientV1
janeDoe = PatientV1 (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 37 68 125 (BloodType O Pos)

-- Listing 12.13. getName, getAge, getBloodType
getName :: PatientV1 -> Name
getName (PatientV1 n _ _ _ _ _) = n

getAge :: PatientV1 -> Int
getAge (PatientV1 _ _ a _ _ _) = a

getBloodType :: PatientV1 -> BloodType
getBloodType (PatientV1 _ _ _ _ _ bt) = bt

-- Listing 12.14. Patient v.2 (with record syntax)
data Patient = Patient { name      :: Name
                       , sex       :: Sex
                       , age       :: Int
                       , height    :: Int
                       , weight    :: Int
                       , bloodType :: BloodType }

jackieSmith :: Patient
jackieSmith = Patient { name      = Name "Jackie" "Smith"
                      , age       = 43
                      , sex       = Female
                      , height    = 62
                      , weight    = 116
                      , bloodType = BloodType O Neg }

-- QUICK CHECK 12.3
-- Q1: Show Jackie Smith’s name.
-- showName (name jackieSmith)

-- Listing 12.15. Updating jackieSmith by using record syntax
jackieSmithUpdated =  jackieSmith { age = 34 }


-- Q12.1 Write a function similar to canDonateTo that takes two patients as
-- arguments rather than two BloodTypes.
canDonateToV2 :: Patient -> Patient -> Bool
canDonateToV2 p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

-- Q12.2 Implement a patientSummary function that uses your final Patient type.
-- patient-Summary should output a string that looks like this:
{-
**************
Patient Name: Smith, John
Sex: Male
Age: 46
Height: 72 in.
Weight: 210 lbs.
Blood Type: AB+
**************
-}
showSex :: Sex -> String
showSex Female = "Female"
showSex Male = "Male"

showLastNameFirst :: Name -> String
showLastNameFirst (Name f l) = l ++ ", " ++ f
showLastNameFirst (NameWithMiddle f m l) = l ++ ", " ++
                                           f ++ " " ++
                                           m ++ "."

showPatient :: Patient -> String
showPatient p = "**************\n" ++
                "Patient Name: " ++ showLastNameFirst (name p) ++ "\n" ++
                "Sex: " ++ showSex (sex p) ++ "\n" ++
                "Age: " ++ show (age p)  ++ "\n" ++
                "Height: " ++ show (height p) ++ "in.\n" ++
                "Weight: " ++ show (weight p) ++ "lbs.\n" ++
                "BloodType: " ++ showBloodType (bloodType p) ++ "\n" ++
                "**************\n"

