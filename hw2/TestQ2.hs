--Due Date: 2017-09-28 23:59:59.999999
--Worked with Patrick Delaney and Anthony Dowling --
-- Author: Tyler Pawlaczyk --
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
addb [] [] 0 = [0]
addb [] [] 1 = [1]
addb [] y c = addb [0] y c
addb x [] c = addb x [0] c
addb (x:xs) (y:ys) c = (sum : (addb xs ys new_c))
    where (sum, new_c) = ( ((x+y+c) `mod` 10), (quot (x+y+c) 10) )

 

{-
convertNumList takes a positive integer
It returns a list of digits in reverse, representing that number

Example:
convertNumList 75
[5,7]

-}
convertNumList :: Int -> Number
convertNumList n
    | n < 0 = []
    | otherwise = reverse (toString (show n))
    where
        toString str
            | str == [] = []
            | otherwise = charToInt (head str) : toString (tail str)
            where
                charToInt c
                    | c == '9' = 9
                    | c == '8' = 8
                    | c == '7' = 7
                    | c == '6' = 6
                    | c == '5' = 5
                    | c == '4' = 4
                    | c == '3' = 3
                    | c == '2' = 2
                    | c == '1' = 1
                    | c == '0' = 0

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
prop_add m n = (addNum m n == addNum n m)


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
-- Fill in your code here
subSum xs n
  | xs == [] = [[]]
  | otherwise =  [ x | x <- permutationsOf xs , sum x == n]
  where 
    permutationsOf (x:xs) = permutationsOf xs ++ [x:subArray | subArray <- (permutationsOf xs)]
    permutationsOf [] = [[]]

{-
Property to partially check subSum using quickCheck
-}
prop_subsum :: [Int] -> Int -> Bool
-- I recommend you fill in your code here
prop_subsum xs n
  | xs == [] = True
  | (sum xs) < n = True
  | subs == [] = True
  | otherwise = sum (head (subs)) == n
  where
    subs = subSum xs n

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
  4. A partial coloring (i.e., a list of pairs of node and oolor)
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
-- Fill in your code here
graphColor colors nodes pairs solution
  = [ r | r <- [ [(y,z) | (y,z) <- x, z /= '\b' ] | x <- rawList ], length r == length nodes ]  -- sorts out invalid solutions
  where 
  rawList
    | length nodes == length solution = [solution]  -- operation done, return the solution
    | otherwise = concat [graphColor colors nodes pairs ((newColored, (colorify newColored colors pairs solution)):solution) |
                          newColored <- nodes,
                          notElem newColored (fst (unzip solution))
                          ]
    where
    colorify node colors pairs solution
      | solution == [] = head colors  -- makes the first node get colored faster
      | validColors == [] = '\b'      -- no valid color could be found
      | otherwise = head validColors  -- takes the first valid color found
      where
      validColors = [ x | x <- colors, notElem x invalidColors] -- finds the legal colors for the node
        where
        invalidColors = [ y | x <- neighbors, y <- colors, (x,y) `elem` solution] -- finds the colors of the neighbors, if there are any
          where
          neighbors = [ if x == node then y else x | (x,y) <- pairs, (x == node) || (y == node)]  -- finds a list of nodes that are neighbors to the node in question

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

{-
Use your encryptChar function from homework 1
-}
encryptChar :: Code -> Char -> Char
-- Fill in your code from homework 1 here
encryptChar code ch
  | code == [] = ch
  | fst (head code) == ch = snd (head code)
  | otherwise = encryptChar (tail code) ch
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
-- Fill in your code here
encryptCycle code cyc
  | (encryptChar code (head cyc)) `elem` cyc = cyc
  | otherwise = encryptCycle code (encryptChar code (head cyc) : cyc)


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
exec :: Program -> Program -> Memory -> Int  
exec prog_whole prog_partial mem
  | tfst cur == "jmp" = exec prog_whole (drop (thrd cur) prog_whole) mem
  | tfst cur == "load" = exec prog_whole (tail prog_partial) (((tsnd cur),(thrd cur)):mem)
  | tfst cur == "add" = exec prog_whole (tail prog_partial) (((tsnd cur),((thrd cur)+(find (tsnd cur) mem))):mem)
  | tfst cur == "blz" = blz prog_whole prog_partial mem (tsnd cur) (thrd cur)
  | tfst cur == "ret" = find (tsnd cur) mem
  | otherwise = (-1)
  where
    cur = head prog_partial
    tfst (x,y,z) = x
    tsnd (x,y,z) = y
    thrd (x,y,z) = z
    find v m
      | (fst(head m)) == v = snd (head m)
      | otherwise = find v (tail m)
    blz prog_whole prog_partial mem v n
      | (find v mem) <= 0 = exec prog_whole (drop n prog_whole) mem
      | otherwise = exec prog_whole (tail prog_partial) mem




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

tylerProg :: Program
tylerProg = [("load","x",4),("load","y",(-1)),("add","x",5),("add","y",1),("blz","y",2),("ret","x",0)]

