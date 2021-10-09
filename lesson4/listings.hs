import Data.List

-- ifEvenInc x = if even x
--               then x + 1
--               else x
ifEvenInc x = ifEven inc x
-- as lambda
-- ifEven (\x -> x+1) 4
-- > 5

-- ifEvenDouble x = if even x
--                  then x * 2
--                  else x
ifEvenDouble x = ifEven double x

-- ifEvenSquare x = if even x
--                  then x^2
--                  else x
ifEvenSquare x = ifEven square x
ifEvenNegate x = ifEven negate x
ifEvenCube   x = ifEven cube x

ifEven f x = if even x
             then f x
             else x

inc x = x + 1
double x = x * 2
square x = x ^ 2
-- negate x = (-) x
cube x = x^3

-- Q1
-- Write a lambda function for cubing x and pass it to ifEven.

-- A1
-- ifEven (\x -> x^3) 4
-- > 64

-- Listing 4.5
-- Q2
names = [("Ian", "Curtis"),
         ("Bernard","Sumner"),
         ("Peter", "Hook"),
         ("Sean", "Morris"),
         ("Ben", "Morris"),
         ("Stephen","Morris")]

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                    then LT
                                    else if firstName1 > firstName2
                                        then GT
                                        else if firstName1 < firstName2
                                            then LT
                                            else EQ
    where lastName1  = snd name1
          lastName2  = snd name2
          firstName1 = fst name1
          firstName2 = fst name2


-- listing 4.6
addressLetter name location = locationFn name
    where locationFn = getLocationFn location

getLocationFn location = case location of
    "ny"  -> nyOffice
    "sf"  -> sfOffice
    "reno"-> renoOffice
    _     -> (\name -> (fst name) ++ " " ++ (snd name))


sfOffice name = if lastName < "L"
                then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

renoOffice name = lastName ++ " - PO Box 456 - Reno, NV 89523"
    where lastName = snd name


