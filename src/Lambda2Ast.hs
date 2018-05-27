{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module : Lambda2Ast
Copyright : (c) 2018 Nicolas Osborne
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
module Lambda2Ast
  ( AST
  , file2ast
  , exp2ast
  ) where

import Utils

-- * Type declaration

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

-- * Main programs

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

-- * Build the Abstract Syntax Tree

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
         
-- * Type manipulations

-- | Given two types and a binary operator, compute the expression's type.
--
-- [@input@]: operator name
-- [@input@]: type of the first argument
-- [@input@]: type of the second argument
-- [@output@]: type of the expression
computeBinaryType :: [Char] -> [Char] -> [Char] -> [Char]
computeBinaryType op t1 t2
  | op == "lambda" = computeAbstractionType t1 t2
  | op == "apply" = computeApplicationType t1 t2
  | op == "pi" = computePairType t1 t2

-- | Given the binder's type and the body's type, compute the type of the
-- abstraction.
--
-- [@input@]: type of the binder.
-- [@input@]: type of the body.
-- [@output@]: type of the expression.
computeAbstractionType :: [Char] -> [Char] -> [Char]
computeAbstractionType t1 t2 = t1 ++ " -> " ++ (parenthesizeType t2)

-- | Given the type of a function and the type of an argument, compute the type
-- of the application.
--
-- [@input@]: the type of the function.
-- [@input@]: the type of the argument.
-- [@output@]: the type of the expression.
computeApplicationType :: [Char] -> [Char] -> [Char]
computeApplicationType t1 t2
  | (isFunction t1) && ( stripPar (head (splitType t1)) == t2) = head (drop 1 (splitType t1))
  | otherwise = "Type Error"

-- | Given the type of two expression, compute the type of the pair.
--
-- [@input@]: the type of the first element of the pair.
-- [@input@]: the type of the second element of the pair.
-- [@output@]: the type of the expression.
computePairType :: [Char] -> [Char] -> [Char]
computePairType t1 t2 = (parenthesizeType t1) ++ " x " ++ (parenthesizeType t2)

-- | Prepare a type to be combined with another type. Put a pair parenthesis
-- around the type if it is a complex type.
--
-- [@input@]: the type
-- [@output@]: the same type
parenthesizeType :: [Char] -> [Char]
parenthesizeType ltype
  | isComplex ltype = "(" ++ ltype ++ ")"
  | otherwise = ltype
  where isComplex :: [Char] -> Bool
        isComplex ltype
          | elem 'x' ltype = True
          | elem '>' ltype = True
          | otherwise = False

-- | Predicate. Determine whether a type is the type of a function.
--
-- [@input@]: a type expression.
-- [@output@]: true if the type is the type of a function, false otherwise.
isFunction :: [Char] -> Bool
isFunction ltype
  | findOp 0 ltype == '>' = True
  | otherwise = False
  where findOp :: Int -> [Char] -> Char
        findOp n [] = 'e'
        findOp n (c:cs)
          | n == 0 && c == '-' = '>'
          | c == '(' = findOp (n+1) cs
          | c == ')' = findOp (n-1) cs
          | otherwise = findOp n cs

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
 
  -- TODO output should be a tuple
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
    splitType' n t1 (s:x:ns)
      | n == 0 && x == '-' = [(stripPar t1), (stripPar (drop 2 ns))]
      | n == 0 && x == 'x' = [(stripPar t1), (stripPar (drop 1 ns))]
      | s == '(' = splitType' (n+1) (t1 ++ [s]) (x:ns)
      | s == ')' = splitType' (n-1) (t1 ++ [s]) (x:ns)
      | otherwise = splitType' n (t1 ++ [s]) (x:ns)

-- * Prepare the program into a list of tokens

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
