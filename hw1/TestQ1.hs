--Due Date: 2017-09-19 23:59:59.999999

--
--Created by Tyler Pawlaczyk. Worked with Patrick Delaney, and Anthony Dowling.
--

module TestQ1 (TestQ1.pairFirst,TestQ1.encryptChar,TestQ1.encryptString,TestQ1.howManyValues,TestQ1.numInvalid,TestQ1.distinctMap,TestQ1.ownInverse,TestQ1.subset,TestQ1.allMapped,TestQ1.mapLetters) where

-- Code is a type synonym
-- it says that a Code is a list of Pairs of Chars
type Code = [(Char,Char)]

-- domain of our code
domain1 :: [Char]
domain1 = ['a'..'z']
domain2 = ['a','b','a']

-- associated range
range1 :: [Char]
range1 = ['z','y'..'a']
range2 = ['a','c','c']

-- Turns two strings into a code
makeCode :: [Char] -> [Char] -> Code
makeCode domain range = zip domain range

-- create a code out of our domain and range
code1 :: Code
code1 = makeCode domain1 range1
code2 = makeCode domain2 range2

-- pairFirst takes a Code and Char
-- returns the list of all Pairs with that Char as first element
pairFirst :: Code -> Char -> Code
pairFirst code ch = [(x,y) | (x,y)<-code, x==ch]


-- uses a code to encrypt a Char
-- if no mapping then the Char encrypts as itself
-- if more than one mapping just use the first value
encryptChar :: Code -> Char -> Char 
encryptChar code ch = if null (pairFirst code ch)
						then ch
						else snd (head (pairFirst code ch)) 

-- uses a Code to encrypt a String
encryptString :: Code -> String -> String
encryptString code chars = [encryptChar code ch | ch<-chars]

-- takes a Code and returns the number of elements a Char is mapped to in that Code
howManyValues :: Code -> Char -> Int
howManyValues code ch = length (pairFirst code ch)


-- takes a Code and returns the number of Chars mapped to more than one element
numInvalid :: Code -> Int
numInvalid code = length  [ count | count <- [ howManyValues code ch | ch <- ['a'..'z'] ] , count > 1]


-- takes a Code and returns True if no Char maps to itself
distinctMap :: Code -> Bool
distinctMap code = null ([(x,y) | (x,y)<-code, x==y])

-- checks if a Code is its own inverse
ownInverse :: Code -> Bool
ownInverse code 
	| encryptString code (encryptString code (fst (unzip code))) == fst (unzip code) = True
	| otherwise = False

-- checks if everything in first String is contained in second String
subset :: String -> String -> Bool 
subset s1 s2 = s1 == [ x | x <- s1, x `elem` s2 ]

-- checks if every letter is the first element of a pair in Code
allMapped :: Code -> Bool
allMapped code = length code == 26 && numInvalid code == 0

-- checks if everything in Code maps to a letter
mapLetters :: Code -> Bool
mapLetters code = length [y | (x,y)<-code, y `elem` ['a'..'z']] == length code
