-- Quick check 5.1 Write a function genIfXEven that creates a closure with x
-- and returns a new function that allows the user to pass in a function to
-- apply to x if x is even.
ifEven f x = if even x
             then f x
             else x

inc x = x + 1
double x = x * 2
square x = x ^ 2

genIfXEven x = (\f -> ifEven f x)

-- listing 5.2
getRequestURL host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey

genHostRequestBuilder host = (\apiKey resource id ->
                             getRequestURL host apiKey resource id)

-- listing 5.3
exampleURLRequestor = genHostRequestBuilder "https://example.com"

-- listing 5.4
apiRequestBuilder hostbuilder apiKey = (\resource id -> hostbuilder apiKey resource  id)

myExampleURLBuilder1 = apiRequestBuilder exampleURLRequestor "1qa2ws3ed"

-- QC 5.2
-- Write a version of genApiRequestBuilder that also takes the resource as an argument.
idRequestBuilder hostbuilder apiKey resource = (\id -> hostbuilder apiKey resource id)

myIDRequestor = idRequestBuilder exampleURLRequestor "1q1a2ws3e3d" "book"

-- listing 5.6
exampleURLBuilder = getRequestURL "https://example.org"
myExampleURLBuilder = exampleURLBuilder "1q1a2w2s3e3d"

-- GC 5.3 Make a builder function that’s specifically for http://example.com,
-- the 1337hAsk3ll API key, and the book resource. That’s a function that
-- requires only the ID of a spe- cific book and then generates the full URL.
exampleIDBuilder = myExampleURLBuilder "book"
altExampleBuilder = getRequestURL "https://example.com" "1q2w3ee3w2q1" "book"

-- listing 5.7
-- from 4.6
addressLetter name location = locationFn name
    where locationFn = getLocationFn location

getLocationFn location = case location of
    "dc"  -> dcOffice
    "ny"  -> nyOffice
    "sf"  -> sfOffice
    "reno"-> renoOffice
    _     -> (\name -> (fst name) ++ " " ++ (snd name))


dcOffice name = nameText ++ ": PO Box 666 - WDC 20003"
    where nameText = fst name ++ " " ++ snd name

sfOffice name = if lastName < "L"
                then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

renoOffice name = lastName ++ " - PO Box 456 - Reno, NV 89523"
    where lastName = snd name

-- flibBinaryArgs f = (\x y -> f y x)
-- addressLetterV2 location name = addressLetter name location
addressLetterV2 = flip addressLetter

addressLetterDC = addressLetterV2 "dc"

-- QC 5.4 Use flip and partial application to create a function called
-- subtract2 that removes 2 from whatever number is passed in to it.
subtract2 = flip (-) 2



