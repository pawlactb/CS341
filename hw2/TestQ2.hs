--Due Date: 2017-09-28 23:59:59.999999

module TestQ2 (Number,addNum,subSum,Color,Node,Edge,Coloring,graphColor,Code,test_cycle,Inst,Variable,Statement,Program,Memory,test_exec) where

import Debug.Trace
import Test.QuickCheck

{-
Digit is a digit in base 10, in other words an Int in [0..9]
-}
type Digit = Int
{-
Number is a list of digits representing a positive integer
  the list will be in reverse (i.e., form lower order to higher order digit)
  the list will not have leading zeroes
-}
type Number = [Digit]

{-
addb takes 3 arguments
  1. A positive integer, represented as a list of digits, in reverse
       (In other words, it goes from lowest order digit to highest order digit,
        and not containing any leading zeroes)
  2. Another positive intger, represented as a list of digits, in reverse
  3. A carry, which will always be 0 or 1
addb returns the result of adding the two positive integers plus the carry

Example:
addb [5,7] [9,8] 0
[4,6,1]

-}
addb :: Number -> Number -> Digit -> Number
-- Fill in your code here
addb [] [] 0 = []
addb [] [] c = [c]
addb (x:xs) [] c 
  | sum < 10   = (sum) : xs
  | otherwise  = (sum - 10) : (addb xs [] 1)
    where sum = x + c
addb [] (y:ys) c
  | sum < 10   = (sum) : ys
  | otherwise  = (sum - 10) : (addb [] ys 1)
    where sum = y + c
addb (x:xs) (y:ys) c
  | sum < 10   = (sum) : (addb xs ys 0)
  | otherwise  = (sum - 10) : (addb xs ys 1)
    where sum = x + y + c
{-
convertNumList takes a positive integer
It returns a list of digits in reverse, representing that number

Example:
convertNumList 75
[5,7]

-}
convertNumList :: Int -> Number
-- Fill in your code here
convertNumList 0 = []
convertNumList n = n `mod` 10 : convertNumList ( n `div` 10 )


{-
addNum takes two integers
It returns the sum of those two integers as a list of digits in reverse

Example:
addNum 75 89
[4,6,1]

-}
addNum :: Int -> Int -> Number
--No need to fill in any code here
addNum m n = addb (convertNumList m) (convertNumList n) 0 

{-
prop_add is a test to check addf using quickCheck
-} 
prop_add :: Int -> Int -> Bool 
-- I recommend that you fill in code here
prop_add m n
  | addNum m n == convertNumList (m + n)   = True
  | otherwise                              = False


{-
subSum takes a list of integer S and another integer n
  (you can assume the numbers in S are distinct)
It returns a list of all subsets of S which add up to n
  (a number cannot be used more than once)
Note: I will call subSum as in the following example:
  take 1 (subSum [2,3,5,7] 12)
So if there is more than one solution, your list does not
  need to contain all of them, as long as it contains at least one

Examples:
subSum [2,3,5,7] 12
[[5,7],[2,3,7]]
subSum [2,3,5,7] 11
[]

-}

subSum :: [Int] -> Int -> [[Int]]
subSum xs n = [ zs | zs <- powerSet xs, sumSet zs == n ]

sumSet :: [Int] -> Int
sumSet [] = 0
sumSet (x:xs) = x + sumSet xs
  
powerSet [] = [[]]
powerSet (x:xs) = previous ++ [x:sub | sub <- previous]
  where previous = powerSet xs

{-
Property to partially check subSum using quickCheck
-}
prop_subsum :: [Int] -> Int -> Bool
prop_subsum xs n
  = True
  

{-
Types used in graph coloring
-}
type Color = Char
type Node = Int
type Edge = (Node,Node)
type Coloring = [(Node,Color)]

{-
graphColor finds all possible colorings for a graph
  a coloring is an assignment of nodes to colors
    such that two nodes must have different colors if they are connected by an edge
graphColor takes the following arguments
  1. A list of colors
  2. A list of nodes
  3. A list of edges (i.e., pairs of nodes)
  4. A partial coloring (i.e., a list of pairs of node and color)
graphColor returns all complete colorings of the graph
If there is more than one solution, your list does not
  need to contain all of them, as long as it contains at least one

Examples:
take 1 (graphColor "abc" [1..6] e1 [])
[[(6,'c'),(5,'a'),(4,'a'),(3,'c'),(2,'b'),(1,'a')]]
take 1 (graphColor "abc" [1..4] e2 [])
[]

-}
graphColor :: [Color] -> [Node] -> [Edge] -> Coloring -> [Coloring]
graphColor [] nodes pairs solution = []
graphColor colors [] pairs solution = []
graphColor colors nodes pairs solution
  | length solution == length nodes    = [solution]
  | otherwise                          = concat [graphColor colors nodes pairs (newColoring:solution) | newColoring <- allColorings, isValid newColoring pairs solution ]
    where allColorings = colorCombinations colors nodes

colorCombinations :: [Color] -> [Node] -> Coloring
colorCombinations colors nodes  = [ (node,color) | node <- nodes, color <- colors ]

otherNode :: Node -> Edge -> Node
otherNode node (n1,n2)
  | node == n1     = n2
  | otherwise      = n1

getNodes :: Node -> [Edge] -> [Node]
getNodes node pairs = [ otherNode node (n1,n2) | (n1,n2) <- pairs, n1 == node || n2 == node ]

invalidColors :: Node -> [Edge] -> Coloring -> [Color]
invalidColors node pairs coloring = [ color | (nodes,color) <- coloring, nodes `elem` ( getNodes node pairs ) ]

isColored :: Node -> Coloring -> Bool
isColored node [] = False
isColored node ((n,c):otherColoring)
  | node == n        = True
  | otherwise        = isColored node otherColoring
  
isValid :: (Node,Color) -> [Edge] -> Coloring -> Bool
isValid (node,color) pairs coloring = not ( isColored node coloring ) && not ( color `elem` invalidColors node pairs coloring )

{-
Example list of edges
-}
e1 :: [Edge]
e1 = [(1,3),(2,1),(2,5),(3,1),(3,2),(3,3),(3,4),(3,5),(5,6),(2,6)]
e2 :: [Edge]
e2 = [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4)]


type Code = [(Char,Char)]

-- domain of our code
domain1 :: [Char]
domain1 = ['a'..'z']

-- associated range
range1 :: [Char]
range1 = ['q'..'z'] ++ ['a'..'p']

-- create a code out of our domain and range
code1 :: Code
code1 = zip domain1 range1

-- pairFirst takes a Code and Char
-- returns the list of all Pairs with that Char as first element
pairFirst :: Code -> Char -> Code
pairFirst code ch 
  = [ (a,b) | (a,b) <- code, a == ch ]


encryptChar :: Code -> Char -> Char 
encryptChar code ch
  | pairFirst code ch == []  = ch
  | otherwise                = head [b | (a,b) <- code, a == ch]
  
  
{-
A cycle is defined as a list of characters c1,c2,...,cn
  such that every character in the sequence is followed by its encryption
A cycle is maximal if the encryption of the last character is already in the list
encryptCycle takes:
  1. a Code
  2. a cycle in reverse
It returns the maximal cycle in reverse starting with the given cycle in reverse

Example:
encryptCycle code1 ['a']
"kueoyiscmwgqa"

-}
encryptCycle :: Code -> [Char] -> [Char]
encryptCycle code cyc
  | newChar `elem` cyc   = cyc
  | otherwise            = encryptCycle code (newChar:cyc)
    where newChar = encryptChar code (head cyc)


{-
test_cycle takes a Code and a character
It returns the maximal cycle starting with that character

Example:
test_cycle code1 'a'
"aqgwmcsiyoeuk"

-}
test_cycle :: Code -> Char -> [Char]
--No need to fill in your code here
test_cycle code ch = reverse (encryptCycle code [ch])

{-
Statement is an assemply language statement with three parts
  1. an instruction i
  2. a variable name v
  3. an integer n
Program is a list of Statements, to be executed in order
  implicitly think of each statement in the program to have a line number
  with the first instruction at line number 0
Memory represents the memory of the computer
  Memory is a list of pairs of a variable and its assigned value
The meaning of each instruction is as follows:
  1. load v n
    give variable v the value n in memory
  2. add v n
    add n to the value of v in memory
  3. jmp v n
    go to line number n, note that v is ignored so anything is allowed
  4. blz v n
    if the value of v is <= 0 then go line n
      otherwise proceed to the next line in the program
  5. ret v n
    quit the program and return the value v, here n is ignored
-}
type Inst = String
type Variable = String 
type Statement = (Inst,Variable,Int)
type Program = [Statement]
type Memory = [(Variable,Int)]  
  
{-
exec executes your program (or a part of your program)
  and returns its return value
A program is executed by executing each instruction in order, except for jmp or blz
exec takes as parameters:
  1. The entire program
  2. The piece of the program that is currently being evaluated
    (i.e., the current instruction up to the end of the program)
It returns the result of the first return statement it encounters
Note 1: The simplest way to update a value is to add a new pair to memorey
  instead of changing the value of what is there
Note 2: I don't care what you do if there are errors, such as:
  1. syntax errors
  2. jumping out of the program
  3. not encountering a return statement

 exec prog1 prog1 []
9

-}

getVariableValue :: Variable -> Memory -> Int
getVariableValue variable memory = head [ val | (var,val) <- memory, var == variable ]

load :: Variable -> Int -> Memory -> Memory
load variable value memory
  = (variable, value):newMemory
    where newMemory = [ (var, val) | (var, val) <- memory, var /= variable ]
	
add :: Variable -> Int -> Memory -> Memory
add variable value memory
  = load variable (value + varValue) memory
    where varValue = getVariableValue variable memory
	
jmp :: Program -> Int -> Program
jmp prog_whole line
  = take (lastInst - (line+1)) (drop line prog_whole)
    where lastInst = length prog_whole + 1
	
blz :: Program -> Program -> Variable -> Memory -> Int -> Program
blz prog_whole prog_partial variable memory line
  | value <= 0        = jmp prog_whole line
  | otherwise         = prog_partial
    where value = getVariableValue variable memory
	
	

exec :: Program -> Program -> Memory -> Int  
-- Fill in your code here
exec prog_whole ((inst, var, val):prog_partial) mem
  | inst == "load"         = exec prog_whole prog_partial memAfterLoad
  | inst == "add"          = exec prog_whole prog_partial memAfterAdd
  | inst == "jmp"          = exec prog_whole progAfterJmp mem
  | inst == "blz"          = exec prog_whole progAfterBlz mem
  | inst == "ret"          = getVariableValue var mem
  | otherwise              = error "Instruction not recognized"
    where { memAfterLoad = load var val mem ;
	        memAfterAdd = add var val mem ;
			progAfterJmp = jmp prog_whole val ;
			progAfterBlz = blz prog_whole prog_partial var mem val }


{-
Initially calling exec function

test_exec prog1
9

-}
test_exec :: Program -> Int
--No need to fill in code here
test_exec p = exec p p []

{-
Example program
-}
prog1 :: Program
prog1 = [("load","x",4),("load","y",5),("load","z",0),
         ("blz","y",7),("add","z",1),("add","y",(-1)),("jmp","",3),
         ("blz","x",11),("add","z",1),("add","x",(-1)),("jmp","",7),("ret","z",0)] 

