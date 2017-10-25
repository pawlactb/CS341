
--Due Date: 2017-10-19 23:59:59.999999

module TestQ3 (search,geogDone,geogNext,graphDone,graphNext,consistent,conflict,lookup',modify,encryptf,encryptString,exec',test_exec') where

import Debug.Trace

{-
search takes the following parameters
  1. A function to check if the search is done
    - this function takes:
      a. the global information for the problem
      b. the partial solution
    - the function returns a bool indicating if solution is complete
  2. A function to find the valid next states to go to
    - this function takes:
      a. the global information for the problem
      b. a partial solution
    - the funtion returns a list of valid next states
  3. Global information for the problem
  4. A partial solution
    - a partial solution is a list of states in reverse
search returns a list of all full solutions reachable from
  the partial solution
    - full solutions will be in forward order
-}
search :: (Eq a, Eq b) =>
  (a -> [b] -> Bool) -> (a -> [b] -> [b]) -> a -> [b] -> [[b]]
--search _ _ _ p | trace ("search  " ++ show p) False = undefined
search done next global partial
  | done global partial == True = [solution]
  | nextPartial == []           = []
  | otherwise                   = search done next global newPartial
    where nextPartial = next global partial
          newPartial = nextPartial ++ partial
          solution = reverseList newPartial  

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []
{-
The following are used to test search for the maze problem
This is already written, there is nothing for you to do here

Global maze information includes:
  1. Size of the maze
  2. List of blocked cells
  3. End of the maze
-}
type Cell = (Int,Int)
type MazeSolution = [Cell]
type MazeGlobal = (Int,[Cell],Cell)

mazeDone :: MazeGlobal -> MazeSolution -> Bool
mazeDone (_,_,end) (cur:_) = end == cur

mazeNext :: MazeGlobal -> MazeSolution -> [Cell]
mazeNext (size,blocked,_) ((x,y):rest)
  =  filter (`notElem` rest) $
     filter (legalMove size blocked)
       [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

legalMove :: Int -> [Cell] -> Cell -> Bool
legalMove size blocked (x,y)
  = notElem (x,y) blocked && x <= size && y <= size && x >=1 && y >= 1

{-
Test cases for the maze problem

Example:

take 1 $ search mazeDone mazeNext (6,blocks2,(6,6)) [(1,1)]
[[(1,1),(1,2),(2,2),(2,3),(2,4),(1,4),(1,5),(1,6),(2,6),(3,6),(4,6),(4,5),(5,5),(6,5),(6,6)]]

 search mazeDone mazeNext (5,blocks1,(5,5)) [(1,1)]
[]

-}

blocks1 :: [(Int,Int)]
blocks1 = [(1,3),(2,1),(2,5),(3,1),(3,2),(3,3),(3,4),(3,5),(5,6),(2,6)]

blocks2 :: [(Int,Int)]
blocks2 = [(1,3),(2,1),(2,5),(3,1),(3,2),(3,3),(3,4),(3,5),(5,6)]

{-
    ++
    +-
    ++
-----+
-+++-+
++-+++
-}

{-
Write some functions to test search for the geography problem
-}

geogDone :: [String] -> [String] -> Bool
geogDone global partial = length global == length partial

geogNext :: [String] -> [String] -> [String]
geogNext global (recent:rest)
  = [ word | word <- global, head word == last recent, word /= recent, word `notElem` rest ]

{-
Test cases for the geography problem

Example:

search geogDone geogNext wl1 ["ant"]
[["ant","tree","elephant","tiger"]]

search geogDone geogNext wl2 ["ab"]
[["ab","ba","acb","bca","aa"],["ab","ba","aa","acb","bca"],["ab","bca","acb","ba","aa"],["ab","bca","aa","acb","ba"]]

-}

wl1 :: [String]
wl1 = ["ant","tiger","elephant","tree"]

wl2 :: [String]
wl2 = ["ab","ba","acb","bca","aa"]

{-
Write some functions to test search for the graph coloring problem
Some types and function headers are already provided
-}

type Color = Char
type Node = Int
{-
If two nodes are connected by an edge
  they cannot be the same color
-}
type Edge = (Node,Node)
{-
A coloring will list all the colored nodes with associated color
  the nodes will be listed in order, starting with 1
  in the search function it will actually be backward
    so ending in 1
    the first node will then be the most recently added
-}
type Coloring = [(Int,Char)]

{-
Global graph coloring informtion consists of:
  1. List of colors 
  2. Largest node in graph, assume nodes are [1..largest]
  3. List of edges
-}
type GraphGlobal = ([Color],Int,[Edge])

graphDone :: GraphGlobal -> Coloring -> Bool
-- TODO
graphDone _ _ = True

graphNext :: GraphGlobal -> Coloring -> [(Node,Color)]
-- TODO
graphNext _ _ = []

{-
consistent takes
  1. A node n
  2. A list of edges
  3. A coloring
  4. A color c
True if node n can be colored with c, given the current coloring
-}
consistent :: Node -> [Edge] -> Coloring -> Color -> Bool 
-- TODO
consistent _ _ _ _ = True

{-
conflict takes
  1. A list of edges
  2. A coloring
  3. A color c
  4. A node n1
  5. A node n2
True if coloring n1 with c would cause a conflict with n2
-}
conflict :: [Edge] -> Coloring -> Color -> Node -> Node -> Bool
-- TODO
conflict _ _ _ _ _ = True


{-
lookup' takes a list of pairs xs and an element y
If x does not appear a s first element of xs then return an error message
Otherwise return the second element of the first time y appears
-}
lookup' :: (Show a, Eq a)  => [(a,b)] -> a -> b
lookup' x _ = (snd . head) x 

{-
Test cases for the graph coloring problem

Example:

search graphDone graphNext ("abc",6,e1) [(1,'a')]
[[(1,'a'),(2,'b'),(3,'c'),(4,'a'),(5,'a'),(6,'c')],[(1,'a'),(2,'b'),(3,'c'),(4,'b'),(5,'a'),(6,'c')],[(1,'a'),(2,'c'),(3,'b'),(4,'a'),(5,'a'),(6,'b')],[(1,'a'),(2,'c'),(3,'b'),(4,'c'),(5,'a'),(6,'b')]]

search graphDone graphNext ("abc",5,e2) [(1,'a')]
[]

-}

e1 :: [Edge]
e1 = [(2,1),(2,5),(3,1),(3,2),(3,4),(3,5),(5,6),(2,6)]
e2 :: [Edge]
e2 = [(1,2),(2,3),(3,4),(1,3),(2,4),(1,4)]

{-
modify takes
  1. an element x of type a
  2. an element y of type b
  3. a function f from a's to b's
it returns a function that is identical to f except it maps x to y
-}
modify :: Eq a => a -> b -> (a -> b) -> (a -> b)
-- Your code goes here
modify _ _ f = f

{-
encryptf takes
  1. a list xs of a's
  2. a list ys of a's
it returns a function that
  maps elements of xs to the correspoding element of ys
  and maps everything else to itself
-}
encryptf :: Eq a => [a] -> [a] -> (a -> a)
-- Your code goes here
encryptf _ _ = \x -> x

{-
encryptString is a way of testing the above function 

encryptString (encryptf "abc" "xyz") "dcba"
"dzyx"

-}
encryptString :: (a -> a) -> [a] -> [a]
-- Your code goes here
encryptString _ _ = []

{-
exec' is the same as exec in the previous assignment
There is only one difference
  instead of representing memory by pairs (Int,Char)
  it represents memory by a function from Int to Char
-}

type Inst = (String,String,Int)  
type Memf = String -> Int

exec' :: [Inst] -> [Inst] -> Memf -> Int  
--exec prog cur mem | trace ("exec " ++ show  (head cur) ++ "  " ++ show mem) False = undefined
exec prog cur mem
-- TODO
exec' _ _ _ = 0

{-
test_exec' calls exec' as in the previous assignment
The only difference is that all variables are should be set to zero
-}
test_exec' :: [Inst] -> Int
-- TODO
test_exec' _ = 0

{-
Test case for test_exec'

Example:

test_exec' prog1
9

-}

prog1 :: [Inst]
prog1 = [("load","x",4),("load","y",5),("load","z",0),
         ("blz","y",7),("add","z",1),("add","y",(-1)),("jmp","",3),
         ("blz","x",11),("add","z",1),("add","x",(-1)),("jmp","",7),("ret","z",0)]

