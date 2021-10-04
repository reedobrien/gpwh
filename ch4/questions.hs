import Data.List

--Q 4.1
-- Anything that can be compared in Haskell (for example, [Char], which you use
-- for the names in your name tuples) can be compared with a function called
-- compare. The compare function returns GT, LT, or EQ. Rewrite
-- compareLastNames by using compare.
names = [("Ian", "Curtis"),
         ("Bernard","Sumner"),
         ("Peter", "Hook"),
         ("Sean", "Morris"),
         ("Ben", "Morris"),
         ("Stephen","Morris")]

compareLastNames name1 name2 = if result == EQ
                               then compare (fst name1) (fst name2)
                               else result
    where result = compare (snd name1) (snd name2)

-- Q 4.2
-- Define a new location function for Washington, DC and add it to
-- getLocation-Function. In the DC function, everyone’s names must be followed
-- by Esq. 
addressLetter name location = locationFn name
    where locationFn = getLocationFn location

getLocationFn location = case location of
    "dc"  -> dcOffice
    "ny"  -> nyOffice
    "reno"-> renoOffice
    "sf"  -> sfOffice
    _     -> (\name -> (fst name) ++ " " ++ (snd name))


dcOffice name = nameText ++ " - PO Box 202 - Washington, DC, 20003"
    where nameText = (fst name) ++ " " ++ (snd name) ++ ", Esq."

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

renoOffice name = lastName ++ " - PO Box 456 - Reno, NV 89523"
    where lastName = snd name

sfOffice name = if lastName < "L"
                then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = fst name ++ " " ++ lastName


