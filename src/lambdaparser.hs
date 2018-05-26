{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module : LambdaParser
Copyrigth : (c) 2018 Nicolas Osborne
Licence : MIT

Provide the function pg2ast which turn a program written in Simply Typed Lambda
Calculus into an Abstract Syntax Tree.

A program in Simply Typed Lambda Calculus is divided into two main parts:

* An @Assignations@ part containing assignations instruction of the form:

    @
    let id Exp
    @

    Where @id@ is an identificator and @Exp@ is a simply styped lambda
    expression.

* A @Program@ part containing the actual computation which may contain @id@
  previously defined in the @Assignations@ part.
-}
module LambdaParser
  ( file2ast
  , exp2ast
  ) where

-- * Type declaration:

-- | Abstract Syntax Tree type declaration.
data AST =
  -- | Correspond to the constant of the language. 
  Leave { name :: String
        , ltype :: String
        , size :: Int -- ^ size of the 'AST'
        } |
  -- | Correspond to the unary operators of the language.
  Unary { name :: String
        , ltype :: String
        , size :: Int
        , below :: AST -- ^ argument of the operator
        } |
  -- | Correspond to the binary operators of the language.
  Binary { name :: String
         , ltype :: String
         , size :: Int
         , left :: AST -- ^ first argument of the binary operator
         , right :: AST -- ^second argument of the binary operator
         } deriving (Show)

-- * Main programs:

-- | Turn a source file into the corresponding 'AST'.
--
-- A source file must contain two main parts deliminated by the three following
--key words in that order: @Assignations@, @Program@, @End@. The assignation part
--(between the two first key words) is a series of assignation instruction (with
--the @let@ key word) and the program part (between the two lasts key words) is
--the actual computation of which the source file is a description. The program
--part may contain the identificators declared in the assignation part.
--
-- [@input@]: The content of the source file.
-- [@output@]: the corresponding 'AST' type checked.
file2ast :: [Char] -> AST
file2ast file = createAST (file2pg file)

-- | Return the corresponding 'AST' of the given expression.
--
-- [@input@]: A well formed expression without any identificator, a "all in one"
-- lambda expression.
-- [@output@]: The corresponding 'AST' type checked.
exp2ast :: [Char] -> AST
exp2ast exp = createAST (words exp)

-- * Build the Abstract Syntax Tree:

-- | Create the 'AST' corresponding to the given list of tokens.
--
-- This function defines the syntax of the language.
--
-- [@input@]: the list of tokens representing the program.
-- [@output@]: the corresponding 'AST' type checked.
createAST :: [[Char]] -> AST
createAST (s:ns)
  | elem s ["lambda","apply","pi"] = let l = createAST ns
                                         r = createAST (drop 1 ns)
                                     in Binary { name = s
                                               , ltype = computeBinaryType s (ltype l) (ltype r)
                                               , size = 1 + (size l) + (size r)
                                               , left = l
                                               , right = r
                                               }
  | elem s ["first","second"] = let b = createAST ns
                              in Unary { name = s
                                       , ltype = computePairElim s (ltype b)
                                       , size = 1 + (size b)
                                       , below = b
                                       }
  | s == "succ" = let b = createAST ns in Unary "succ" (ltype b) (1+(size b)) b
  | s == "zero" = Leave "zero" "nat" 1
  | s == "true" = Leave "true" "bool" 1
  | s == "false" = Leave "false" "bool" 1
  | s == "null" = Leave "null" "empty" 1
  | length s == 1 = Leave s "char" 1
  | elem ':' s = let splitedVar = (oneSplit ':' s)
                     n = fst splitedVar
                     t = snd splitedVar
                 in Leave n t 1
  | otherwise = Leave "Error" "Error" 1
         
-- * Type manipulations:

-- | Given two types and a binary operator, compute the expression's type.
--
-- [@input@]: operator name
-- [@input@]: type of the first argument
-- [@input@]: type of the second argument
-- [@output@]: type of the expression
computeBinaryType :: [Char] -> [Char] -> [Char] -> [Char]
computeBinaryType "lambda" t1 t2 = "(" ++ t1 ++ ">" ++ t2 ++ ")"
computeBinaryType "apply" t1 t2
  | elem '>' t1 && (head subtypes) == t2 = head (drop 1 subtypes)
  | otherwise = "type error"
  where subtypes = splitType t1
computeBinaryType "pi" t1 t2 = "(" ++ t1 ++ "x" ++ t2 ++ ")"

-- | Compute type of a pair elimination
--
-- [@input@]: operator, either @first@ or @second@
-- [@input@]: type of the pair, /ie/ of the from @AxB@
-- [@output@]: type of the expression
computePairElim :: [Char] -> [Char] -> [Char]
computePairElim op pairType
  | op == "first" = head (splitType pairType)
  | op == "second" = head (drop 1 (splitType pairType))
  | otherwise = "Type Error"

-- * Prepare the program into a list of tokens:

-- | Prepare the content of a file for 'createAST' function.
--
-- [@input@]: The content of a file as described in the specifications.
-- [@output@]: A list of tokens ready for building an AST.
file2pg :: [Char] -> [[Char]]
file2pg file = let listTokens = file2list file
                   partA = extractAssignations listTokens
                   partPg = extractPg listTokens
               in preparePg (parseA partA) partPg

-- TODO avoid use of length by computing length res when building res
-- in order to do that: read :: x -> y -> [] -> (Int, [])
-- | Turn a String into the corresponding list of tokens.
--
-- Split the string and drop the separators.
-- Separators are whitespace and end of line.
file2list :: [Char] -> [[Char]]
file2list pg = mydbsplit ' ' '\n' pg
  where 
    mydbsplit :: (Eq a) => a -> a -> [a] -> [[a]]
    mydbsplit x y [] = []
    mydbsplit x y (c:cs)
      | c == x = mydbsplit x y cs
      | c == y = mydbsplit x y cs
      | otherwise = [res] ++ mydbsplit x y (drop (length res) cs)
      where res = read x y (c:cs)
            read x y [] = []
            read x y (c:cs) 
              | c == x = []
              | c == y = []
              | otherwise = c:read x y cs

-- TODO handle commentary section before Assignations
-- | Return the sub-list corresponding to the list of tokens corresponding to
-- the Assignation part of the source file.
extractAssignations :: [[Char]] -> [[Char]]
extractAssignations [] = []
extractAssignations (s:ns)
  | s == "Assignations" = extractAssignations ns
  | s == "Program" = []
  | otherwise = s:extractAssignations ns

-- | Return the sub-list corresponding to the list of tokens corresponding to
-- the Program part of the source file
extractPg :: [[Char]] -> [[Char]]
extractPg [] = []
extractPg (s:ns)
  | s == "Program" = stripEnd ns
  | otherwise = extractPg ns
  where stripEnd (w:ws)
          | w == "End" = []
          | otherwise = w:stripEnd ws
          
  -- TODO use Data.HashMap.Strict for better time complexity
-- | Parse the Assignation part of the source file.
--
-- [@Input@]: The list of tokens corresponding to the Assignation part of the
-- source file.
-- [@Output@]: A list of assignations. The @head@ of each item is an @id@ and
-- the @tail@ is the @expression@
parseA :: [[Char]] -> [[[Char]]]
parseA tokens = oneSplit "let" tokens
  where oneSplit str [] = []
        oneSplit str (s:ns)
          | s == str = oneSplit s ns
          | otherwise = [res] ++ oneSplit str (drop (length res) ns)
          where res = read str (s:ns)
                read str [] = []
                read str (s:ns)
                  | s == str = []
                  | otherwise = s:read str ns

-- | Substitute the Id in the list of tokens corresponding to the program part
-- of the source file by the corresponding list of tokens.
preparePg :: [[[Char]]] -> [[Char]] -> [[Char]]
preparePg _ [] = []
preparePg list1 (x:xs) = (check list1 x) ++ (preparePg list1 xs)
  where check :: [[[Char]]] -> [Char] -> [[Char]]
        check [] x = [x]
        check (y:ys) x
          | x == head y = tail y
          | otherwise = check ys x
         
-- * Utils:

-- | Split a string into two substrings, the prefix up to the first occurrence
-- of the given Char, and the suffix beginning after this occurrence.
--
-- [@input@]: the separator
-- [@input@]: the string
-- [@output@]: the pair of the two substrings
oneSplit :: Char -> [Char] -> ([Char], [Char])
oneSplit c str = (fst res, drop (1+(snd res)) str)
  where res = substring 0 c [] str
        substring :: Int -> Char -> [Char] -> [Char] -> ([Char], Int)
        substring n c w [] = (w, n)
        substring n sep w (c:cs)
          | c == sep = (w, n)
          | otherwise = substring (n+1) sep (w++[c]) cs
 
-- | Split a composed type in its two main parts.
--
-- [@input@]: a composed type
-- [@output@]: the list of the two types composing the composed type
splitType :: [Char] -> [[Char]]
splitType [] = []
splitType t = splitType' 0 [] t
  where 
    splitType' :: Int -> [Char] -> [Char] -> [[Char]]
    splitType' 0 t1 [] = [stripPar t1]
    splitType' _ t1 [] = ["Type Error"]
    splitType' n t1 (s:ns)
      | n == 0 && (elem s ">x") = [(stripPar t1), (stripPar ns)]
      | s == '(' = splitType' (n+1) (t1 ++ [s]) ns
      | s == ')' = splitType' (n-1) (t1 ++ [s]) ns
      | otherwise = splitType' n (t1 ++ [s]) ns
               
-- | strip a string from its outer parenthesis if there is any
stripPar :: [Char] -> [Char]
stripPar (c:cs)
  | c == '(' = take ((length cs) - 1) cs
  | otherwise = c:cs
