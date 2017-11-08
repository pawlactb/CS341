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
search :: (Show a, Show b) =>
  (a -> [b] -> Bool) -> (a -> [b] -> [b]) -> a -> [b] -> [[b]]
--search _ _ _ p | trace ("search  " ++ show p) False = undefined
search isDone nextMoves global partial
	| isDone global partial           = [reverse partial]
	| otherwise                       = concat $ map (search isDone nextMoves global) nextStates
	      where nextStates = [nextState:partial | nextState <- nextMoves global partial]

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
geogDone global partial
  = (length global) == (length partial)

geogNext :: [String] -> [String] -> [String]
geogNext [] partial = partial
geogNext global [] = global
geogNext global partial@(w:_)
  = [x | x <- global, notElem x partial, isValid x w]

-- Defining isValid
isValid :: String -> String -> Bool
isValid x w
  = (head x) == (last w)

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
graphDone (_,numNodes,_) partial = length partial == numNodes

graphNext :: GraphGlobal -> Coloring -> [(Node,Color)]
graphNext (colors, numNodes, edges) partial 
  | nextNode == 0              = [] 
  | otherwise                  = [(nextNode, color) | color <- colors,
                                     consistent nextNode edges partial color]
	        where nextNode = getNextNode partial numNodes


-- Grabs the next available node (goes up by one each time)
getNextNode :: Coloring -> Int -> Int
getNextNode [] numNodes
  | numNodes < 1     = 0
  | otherwise         = 1

getNextNode ((node,_):rest) numNodes
  | nextNode > numNodes   = 0
  | otherwise             = nextNode
        where nextNode = node+1
  
  
  
--checks if node was already colored
alreadyColored :: Node -> Coloring -> Bool
alreadyColored node [] = False
alreadyColored node ((n,c):restColoring)
  | node == n         = True
  | otherwise         = alreadyColored node restColoring



-- Grabs the node that's opposite the given node in a pair
-- ASSUMPTION: The pair contains the given node
extractEdgeNode :: Edge -> Node -> Node
extractEdgeNode (n1, n2) node
    | n1 /= node        = n1
    | otherwise         = n2

-- Grabs all edges that have our node as either the first or second element
getEdgeNodes :: Node -> [Edge] -> [Node]
getEdgeNodes node pairs
    = [extractEdgeNode (n1, n2) node | (n1, n2) <- pairs, n1 == node || n2 == node]



{-
consistent takes
  1. A node n
  2. A list of edges
  3. A coloring
  4. A color c
True if node n can be colored with c, given the current coloring
-}
consistent :: Node -> [Edge] -> Coloring -> Color -> Bool 
consistent node edges partial color
  = not (alreadyColored node partial) && not (conflictEdges edgeNodes)
         where { edgeNodes = getEdgeNodes node edges ;
		         conflictEdges = foldl (\acc n -> (isConflict n) || acc) False ;
				 isConflict = conflict edges partial color node
               }

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
conflict edges partial color n1 n2
  = (n2 `elem` edgeNodes) && (invalidColor color partial n2)
      where edgeNodes = getEdgeNodes n1 edges


-- Checks if color is invalid by looking if n1 has already claimed it
invalidColor :: Color -> Coloring -> Node -> Bool
invalidColor color partial n1
  | inList partial n1      = color == (lookup' partial n1)
  | otherwise              = False


-- Checks if element is in list
inList :: (Show a, Eq a) => [(a,b)] -> a -> Bool
inList list y
  = foldl (\acc (a,b) -> acc || (a == y)) False list


{-
lookup' takes a list of pairs xs and an element y
If x does not appear a s first element of xs then return an error message
Otherwise return the second element of the first time y appears
-}
lookup' :: (Show a, Eq a)  => [(a,b)] -> a -> b
lookup' ((a,b):rest) y
  | y == a             = b
  | otherwise          = lookup' rest y

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
modify x y f = (\z -> if z == x then y else f z)

{-
encryptf takes
  1. a list xs of a's
  2. a list ys of a's
it returns a function that
  maps elements of xs to the correspoding element of ys
  and maps everything else to itself
-}
encryptf :: Eq a => [a] -> [a] -> (a -> a)
encryptf xs ys = (\x -> mapIt xs ys x)


-- mapIt maps a character to it's corresponding thing and stuff
mapIt :: Eq a => [a] -> [a] -> a -> a
mapIt [] _ z = z
mapIt (x:xs) [] z = z
mapIt (x:xs) (y:ys) z
  | z == x     = y
  | otherwise  = mapIt xs ys z

{-
encryptString is a way of testing the above function 

encryptString (encryptf "abc" "xyz") "dcba"
"dzyx"

-}
encryptString :: (a -> a) -> [a] -> [a]
encryptString f [] = []
encryptString f (x:xs) = (f x):(encryptRest)
  where encryptRest = encryptString f xs

{-
exec' is the same as exec in the previous assignment
There is only one difference
  instead of representing memory by pairs (Int,Char)
  it represents memory by a function from Int to Char
-}

type Inst = (String,String,Int)  
type Memf = String -> Int

-- Adds variable to memory function
load :: String -> Int -> Memf -> Memf
load var val memf
  = modify var val memf


-- Changes variable in memory function
add :: String -> Int -> Memf -> Memf
add var val memf
  = modify var newVal memf
      where newVal = val + (memf var)

-- Returns a prog_partial starting at the specified line number
jump :: [Inst] -> Int -> [Inst]
jump prog_whole lineNum
   = take (lastIns - (lineNum+1)) (drop lineNum prog_whole)
         where lastIns = length prog_whole + 1


-- Jumps to lineNum if value of variable is <= 0
blz :: [Inst] -> [Inst] -> String -> Memf -> Int -> [Inst]
blz prog_whole prog_partial variable memf lineNum
  | value <= 0        = jump prog_whole lineNum
  | otherwise         = prog_partial
       where value = memf variable


-- Main execution function
exec' :: [Inst] -> [Inst] -> Memf -> Int  
--exec prog cur mem | trace ("exec " ++ show  (head cur) ++ "  " ++ show mem) False = undefined 
-- New memory function is returned using add and load functions
exec' prog_whole ((inst, var, val):prog_partial) memf
  | inst == "load"        = exec' prog_whole prog_partial memfAfterLoad
  | inst == "add"         = exec' prog_whole prog_partial memfAfterAdd
  | inst == "jmp"         = exec' prog_whole progAfterJmp memf
  | inst == "blz"         = exec' prog_whole progAfterBlz memf
  | inst == "ret"         = memf var
  | otherwise             = error "Instruction not recognized in program"
       where { memfAfterLoad = load var val memf ;
               memfAfterAdd = add var val memf ;
               progAfterJmp = jump prog_whole val ;
               progAfterBlz = blz prog_whole prog_partial var memf val }


{-
test_exec' calls exec' as in the previous assignment
The only difference is that all variables are should be set to zero
-}
test_exec' :: [Inst] -> Int
test_exec' program
  = exec' program program initialMemf
       where initialMemf = (\x -> 0)

{-
Test case for test_exec'

Example:

test_exec' prog1
9

-}

prog1 :: [Inst]
prog1 = [("load","x",5),("load","y",5),("load","z",-5),
         ("blz","y",7),("add","z",1),("add","y",(-1)),("jmp","",3),
         ("blz","x",11),("add","z",1),("add","x",(-1)),("jmp","",7),("ret","z",0)]
