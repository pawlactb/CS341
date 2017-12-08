--Due Date: 2017-12-08 23:59:59.999999
module TestQ5 (run) where
import Debug.Trace
import Data.Char
import Parse


{-
A statement is one of the following
  1. An if-else statement
     When we read it in, it is of the form:
       if (condition) statement else statement
  2. A while statement
     When we read it in, it is of the form:
       while (condition) statement
  3. An assigment statement
     When we read it in, it is of the form:
       variable = expression;
  4. A block of statements
     When we read it in, it is of the form:
       { statement statement ... statement }
     with zero or more statements in curly brackets
  5. A declaration of a variable
     When we read it in, it is of the form:
       int variable;
     so the only data type is integer
     A variable is initialized as zero when declared
     A variable is made up entirely of letters
-}
data Statement = IfElse Condition Statement Statement |
                 While Condition Statement |
                 Assign Expression Expression |
                 Block [Statement] |
                 Declare Expression
  deriving Show

{-
A condition is read in as one of the following forms:
  1. expression < expression
  2. expression > expression
  3. expression <= expression
  4. expression >= expression
  5. expression == expression
  6. expression != expression
  7. condition && condition
  8. condition || condition
  9. ! condition
You can assume that a condition will contain at most one
  boolean operator (&&,||,!)
So you don't have to worry about precedence or associativity
-}
data Condition = Less Expression Expression |
                 Greater Expression Expression |
                 LessEq Expression Expression |
                 GreaterEq Expression Expression |
                 Equal Expression Expression |
                 NotEqual Expression Expression |
                 And Condition Condition |
                 Or Condition Condition |
                 Not Condition
  deriving Show

{-
An expression is read in as one of the folowing forms:
  1. expression + expression
  2. expression - expression
  3. expression * expression
  4. expression / expression
  5. variable
  6. number
-}
data Expression = Plus Expression Expression |
                   Minus Expression Expression |
                   Times Expression Expression |
                   Divide Expression Expression |
                   Var String |
                   Num Int
  deriving Show

{-
Memory is a set of pairs consisting of
  - a variable
  - the current value of that variable
Variables could be duplicated in memory
  then I will assume the first occurence
  of a variable gives the current value
-}
type Memory = [(String,Int)]

{-
This function will parse your input and run the program
A program is a list of statements surrounded by curly brackets
  in other words, a program is a statement
When you run your program, initially the memory is empty
This function will return the memory when the program is completed
-}
run :: String -> Memory
run string = evalStmt (parse stmt string) []

{-
To evaluate a statement you give
  1. the statement
  2. the current memory
It returns the memory after the statement is executed
-}
evalStmt :: Statement -> Memory -> Memory 
evalStmt stmt mem | trace ("evalStmt \n" ++ show stmt ++ "  " ++ show mem) False = undefined
evalStmt (IfElse cond ifTrue ifFalse) mem = if (evalCond cond mem) then (evalStmt ifTrue mem) else (evalStmt ifFalse mem)
evalStmt (Assign var x) mem
  | (show var, 0) `elem` mem = (show var, read $ show x):mem
  | otherwise                = error err
   where err = "Variable " ++ show var ++ " not declared!"
evalStmt (While c s) mem
  | evalCond c mem = evalStmt (While c s) (evalStmt s mem)
  | otherwise = mem
evalStmt (Block []) mem = mem
evalStmt (Block (x:xs)) mem = (evalStmt x (evalStmt (Block xs) mem)) 
evalStmt _ _ = []

{-
To evaluate a condition you give
  1. the condition
  2. the current memory
It returns a bool indicating if the condition is true
-}
evalCond :: Condition -> Memory -> Bool
evalCond (Less x y) mem = (evalExp x mem) < (evalExp y mem)
evalCond (Greater x y) mem = (evalExp x mem) > (evalExp y mem)
evalCond (LessEq x y) mem = (evalExp x mem) <= (evalExp y mem)
evalCond (GreaterEq x y) mem = (evalExp x mem) >= (evalExp y mem)
evalCond (Equal x y) mem = (evalExp x mem) == (evalExp y mem)
evalCond (NotEqual x y) mem = (evalExp x mem) /= (evalExp y mem)
evalCond (And x y) mem = (evalCond x mem) && (evalCond y mem)
evalCond (Or x y) mem = (evalCond x mem) || (evalCond y mem)
evalCond (Not x) mem = not (evalCond x mem)


{-
To evaluate an expression you give
  1. the expression
  2. the current memory
It returns the value of the expression
-}
evalExp :: Expression -> Memory -> Int
evalExp (Num n) mem = n
evalExp (Plus x y) mem = evalExp x mem + evalExp y mem
evalExp (Minus x y) mem = evalExp x mem - evalExp y mem
evalExp (Times x y) mem = evalExp x mem * evalExp y mem
evalExp (Divide x y) mem = evalExp x mem `div` evalExp y mem
evalExp (Var v) mem
  | maybeval == Nothing = error $ v ++ " has no value!"
  | otherwise           = val
  where maybeval = lookup v mem
        Just val = maybeval

-- This parses a statement and stores the result
stmt :: Parse Char Statement
stmt = ((token '(' <*< tokens "int" <*< var >*< tokens ";)") `build` (\x -> Declare x)) `alt`
       ((token '(' <*< var >*> token '=' <*< expr >*< tokens ";)") `build` (\(x,y) -> Assign x y)) `alt`
       ((tokens "({" <*< (list stmt)  >*< tokens "})") `build` (\xs -> Block xs)) `alt`
       ((tokens "((while" <*< cond >*> token ')' <*< stmt >*< token ')') `build` (\(x,y) -> While x y)) `alt`
       ((tokens "(if(" <*< cond >*> token ')' <*< stmt >*> tokens "(else)" <*< stmt >*< token ')') `build` (\(x,(y,z)) -> IfElse x y z))

-- This parses a condition and stores the result
cond :: Parse Char Condition
-- fill in your code here
cond = ((token '(' <*< expr >*> token '<' <*< expr >*< token ')') `build` (\(x,y) -> Less x y)) `alt`
       ((token '(' <*< expr >*> token '>' <*< expr >*< token ')') `build` (\(x,y) -> Greater x y)) `alt`
	   ((token '(' <*< expr >*> tokens "<=" <*< expr >*< token ')') `build` (\(x,y) -> LessEq x y)) `alt`
	   ((token '(' <*< expr >*> tokens "<=" <*< expr >*< token ')') `build` (\(x,y) -> GreaterEq x y)) `alt`
	   ((token '(' <*< expr >*> tokens "==" <*< expr >*< token ')') `build` (\(x,y) -> Equal x y)) `alt`
	   ((token '(' <*< expr >*> tokens "!=" <*< expr >*< token ')') `build` (\(x,y) -> NotEqual x y)) `alt`
	   ((token '(' <*< cond >*> tokens "&&" <*< cond >*< token ')') `build` (\(x,y) -> And x y)) `alt`
	   ((token '(' <*< cond >*> tokens "||" <*< cond >*< token ')') `build` (\(x,y) -> Or x y)) `alt`
	   (token '(' <*< token '!' <*< cond >*< token ')') `build` (\x -> Not x)

-- This parses an expression and stores the result
expr :: Parse Char Expression
-- fill in your code here
expr = num `alt` var `alt`
       ((token '(' <*< expr >*> token '+' <*< expr >*< token ')') `build` (\(x,y) -> Plus x y)) `alt`
	   ((token '(' <*< expr >*> token '-' <*< expr >*< token ')') `build` (\(x,y) -> Minus x y)) `alt`
	   ((token '(' <*< expr >*> token '*' <*< expr >*< token ')') `build` (\(x,y) -> Times x y)) `alt`
	   ((token '(' <*< expr >*> token '/' <*< expr >*< token ')') `build` (\(x,y) -> Divide x y))

num :: Parse Char Expression
num = (spot isDigit >*> (list (spot isDigit))) `build` (uncurry (:))
      `build` (\x -> Num (read x :: Int))

var :: Parse Char Expression
var = (spot isAlpha >*> (list (spot isAlpha))) `build` (uncurry (:))
      `build` Var
