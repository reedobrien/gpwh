-- Listing 15.1 Defining a four-letter alphabet
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

-- Listing 15.2. A generic rotN function to work on any alphabet
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where halfAlphabet = alphabetSize `div` 2
        offset = fromEnum c + halfAlphabet
        rotation = offset `mod` alphabetSize

-- Listing 15.3. Getting the number representing the largest Char
largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

-- smallestCharNumber :: Int
-- smallestCharNumber :: fromEnum (minBound :: Char)

-- Listing 15.4. Rotating a single Char
rotChar :: Char -> Char
rotChar char = rotN sizeOfAlpha char
  where sizeOfAlpha = 1 + fromEnum (maxBound :: Char)
-- '\557153'
-- rotChar 'a'

-- Listing 15.5. A message in your four-letter alphabet
message :: [FourLetterAlphabet]
message = [L1,L3,L4,L1,L1,L2]

-- Listing 15.6. Defining a fourLetterEncoder with map
fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot41 vals
  where alphaSize = 1 + fromEnum (maxBound:: FourLetterAlphabet)
        rot41     = rotN alphaSize

fourLetterAlphabetDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetDecoder vals = map rot41decoder vals
  where alphaSize    = 1 + fromEnum (maxBound ::FourLetterAlphabet)
        rot41decoder = rotNdecoder alphaSize

-- Listing 15.7. A three-letter alphabet, message, and encoder
data ThreeLetterAlphabet = Alpha
                         | Beta
                         | Kappa deriving (Show, Enum, Bounded)

threeLetterMessage :: [ThreeLetterAlphabet]
threeLetterMessage = [Alpha, Alpha , Beta, Alpha, Kappa]

threeLetterEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterEncoder vals = map rot31 vals
  where alphaSize = 1 + fromEnum(maxBound::ThreeLetterAlphabet)
        rot31     = rotN alphaSize

-- Listing 15.8. A rotNdecoder that works with odd-numbered alphabets
rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where halfN = n `div` 2
        offset = if even n
                 then fromEnum c + halfN
                 else 1 + fromEnum c + halfN
        rotation = offset `mod` n

threeLetterDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetterDecoder vals = map rot31decoder vals
             where alphaSize = 1 + fromEnum (maxBound :: ThreeLetterAlphabet)
                   rot31decoder = rotNdecoder alphaSize

-- Listing 15.10. Rotating strings with rotEncoder and rotDecoder
rotEncoder :: String -> String
rotEncoder text = map rotChar text
  where alphaSize = 1 + fromEnum (maxBound::Char)
        rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text = map rotCharDecoder text
  where alphaSize = 1 + fromEnum (maxBound :: Char)
        rotCharDecoder = rotNdecoder alphaSize

-- Listing 15.11. xorBool, a foundation for xor
xorBool :: Bool -> Bool -> Bool
xorBool v1 v2 = (v1 || v2) && (not (v1 && v2))

-- Listing 15.12. xorPair to xor pairs of Bools
xorPair :: (Bool, Bool) -> Bool
xorPair (v1, v2) = xorBool v1 v2

-- Listing 15.13. Finally, your completed xor function
xor :: [Bool] -> [Bool] -> [Bool]
xor l1 l2 = map xorPair (zip l1 l2)

-- Listing 15.14 Bits type synonym
type Bits = [Bool]

-- Listing 15.15. intToBits' starting to convert an Int type to Bits
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
              then False : intToBits' next
              else True : intToBits' next
    where remainder = n `mod` 2
          next      = n `div` 2

-- Listing 15.16. maxBits and your final intToBits function
maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingFalse ++ reversedBits
  where reversedBits = reverse (intToBits' n)
        missingBits  = maxBits - (length reversedBits)
        leadingFalse = take missingBits (cycle [False])

-- Listing 15.17. charToBits to convert Chars into Bits
charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char)


-- Listing 15.18. bitsToInt to go backward from Bits to an Int type
bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
  where size = length bits
        indices = [size-1, size-2 .. 0]
        trueLocations = filter (\x -> fst x == True)
                        (zip bits indices)

-- Listing 15.19. Completing the transformation by going back from bitsToChar
bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

-- Listing 15.20. A simple pad
myPad :: String
myPad = "Shhhhh"

-- Listing 15.21. Your plain text
myPlainText :: String
myPlainText = "Haskell"

-- Listing 15.22. applyOTP' for converting a string to bits with a one-time pad
applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\pair ->
                                (fst pair) `xor` (snd pair))
                          (zip padBits plainTextBits)
  where padBits       = map charToBits pad
        plainTextBits = map charToBits plainText

-- Listing 15.23. Finally, applyOTP to encode strings using a one-time pad
applyOTP :: String->String->String
applyOTP pad plainText = map bitsToChar bitList
  where bitList = applyOTP' pad plainText

-- Listing 15.24. Partial application to create an encoderDecoder
encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

-- Listing 15.25. A Cipher class to generalize your cipher operations
class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

-- Listing 15.26. The Rot data type
data Rot = Rot

-- Listing 15.27. Making Rot an instance of Cipher
instance Cipher Rot where
  encode Rot text = rotEncoder text
  decode Rot text = rotDecoder text

-- Listing 15.28. The OneTimePad data type
data OneTimePad = OTP String

-- Listing 15.29. Making OneTimePad an instance of Cipher
instance Cipher OneTimePad where
  encode (OTP pad) text = applyOTP pad text
  decode (OTP pad) text = applyOTP pad text

-- Listing 15.30. Using lazy evaluation to create a limitless pad
myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

-- Listing 15.31. A linear congruential PRNG
prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a*seed + b) `mod` maxNumber

-- -- Listing 15.32. examplePRNG
examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

-- Extending the exercise
-- To explore this on your own, use the PRNG to create a StreamCipher type that
-- can be an instance of the Cipher class. Remember: never use your own crypto
-- in the real world! Assume that this should be used for passing notes only.

-- Define a stream cipher instance of Cipher.
instance Cipher StreamCipher where
    encode (StreamCipher a b max iv) text = streamCipher text a b max iv
    decode = encode

-- StreamCipher is a data type constructed with a constructor of the same name
-- which takes 4 integers.
data StreamCipher = StreamCipher Int Int Int Int
--
-- streamCipher encodes/decodes text using streamCipher'
streamCipher :: String -> Int -> Int -> Int -> Int -> String
streamCipher plaintext a b n iv = map bitsToChar (streamCipher' plaintext a b n iv)

-- streamCipher' is a helper function that encodes/decodes text into a list of bits
streamCipher' :: String -> Int -> Int -> Int -> Int -> [Bits]
streamCipher' plaintext a b max iv  = map (\pair -> (fst pair) `xor` (snd pair))
                                          (zip streamBits plaintextBits)
    where
        plaintextBits = map charToBits plaintext
        streamBits = genRandom a b max iv

-- genRandom converts getnInts' output into Bits.
genRandom :: Int -> Int -> Int -> Int -> [Bits]
genRandom a b max iv = map intToBits (genInts' a b max iv)

-- genInts' creates a list of integers.
genInts' :: Int -> Int -> Int -> Int -> [Int] 
genInts' a b max iv = if max == 0
                      then []
                      else iv : genInts' a b (max-1) s
    where s = prng a b max iv

-- Usage 
-- sc = StreamCipher 1337 7 100 987203
-- encode sc "Haskell"
-- "\987147scHU[`"
-- decode sc "\987147scHU[`"
-- "Haskell"

