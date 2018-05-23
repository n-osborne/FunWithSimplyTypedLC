{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module : LambdaParser
Copyrigth : Nicolas Osborne, 2018
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
  ( pg2ast ) where



-- Type declaration and main program:

-- | Abstract Syntax Tree type declaration.
--
-- to be completed
data AST = Empty |
           Leaf { len :: Int -- ^ length of the leaf, which is obviously 1
                , lambdatype :: [Char] -- ^ One of the defined type of the
                                -- language
                , name :: [Char] -- ^ Either a value or an id
                } deriving (Show)


-- | Turn a source file into the corresponding AST.
--
-- A source file must contain two main parts deliminated by the three following
--key words in that order: @Assignations@ @Program@ @End@. The assignation part
--(between the two first key words) is a series of assignation instruction (with
--the @let@ key word) and the program part (between the two lasts key words) is
--the actual computation of which the source file is a description. The program
--part may contain the identificators declared in the assignation part.
--
-- [@input@]: The content of the source file.
--
-- [@output@]: the corresponding @AST@.
pg2ast :: [Char] -> AST
pg2ast pg = createAST list_tokens_pg_with_subst
  where list_tokens_pg_with_subst = preparePg list_assignations list_tokens_pg
        list_assignations = parseA extractAssignations pg
        list_tokens_pg = selectProgram pg



-- Helper functions

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


{--
would be version with import Data.List.Split
file2list pg = split (condens . dropDelims oneOf "\n ") pg
-}

  
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
          
-- | Parse the Assignation part of the source file.
--
-- [@Input@]: The list of tokens corresponding to the Assignation part of the
--source file.
--
-- [@Output@]: A list of Product Id x Exp

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



-- | Create the AST corresponding to the given list of tokens.
createAST [[Char]] -> AST






{--

import GHC.Unicode

-- | Abstract Syntax Tree type declaration.
--
-- An Ast is either an empty tree (correspond to an empty program), or a Node
-- with two children which are Abstract Syntax Trees with leaves.
-- Nodes are for lambda application, so they have an arity of two.
-- Leaves are for names.
data Ast = Empty |
           Node { len :: Int -- ^ length of the tree
                , operator :: [Char] -- ^ tag for the operator, either App or Abs
                , left :: Ast -- ^ either a binder or a lambda abstraction
                , rigth :: Ast -- ^ a lambda expression
                } |
           Leaf { len :: Int -- ^ length of the leaf, which is obviously 1
                , name :: [Char] -- ^ name: id:Type
                } deriving (Show)

-- | Turn a program into the corresponding Abstract Syntax Tree.
pg2ast :: [Char] -> Ast
pg2ast pg = createAst (parsePg pg)

-- | Turn a program into the corresponding list of its tokens.
parsePg :: [Char] -> [[Char]]
parsePg [] = []
parsePg (c:cs)
  | c == '/' = "Abs" : parsePg cs
  | c == '(' = "App" : parsePg cs
  | c == ')' = parsePg cs
  | c == '.' = parsePg cs
  | c == ' ' = parsePg cs
  | otherwise = name : parsePg (drop (length name) (c:cs))
  where name = readName (c:cs)


-- | Turn a list of the tokens of a program into the corresponding Abstact
-- Syntax Tree.
createAst :: [[Char]] -> Ast
createAst [] = Empty
createAst (c:cs)
  | c == "Abs" = Node { len=n, operator=c, left=lAst, rigth=rAst }
  | c == "App" = Node { len=n, operator=c, left=lAst, rigth=rAst }
  | otherwise = Leaf { len=1, name=c }
  where lAst = createAst cs
        rAst = createAst (drop (len lAst) cs)
        n = 1 + len lAst + len rAst

-- | Extract the string corresponding of a name from the point of the program we
-- have reached in the parsing.
readName :: [Char] -> [Char]
readName [] = []
readName s
  | length s >= 3 = readId s
  | otherwise = "Error"

-- | Check that the id is lower case.
readId :: [Char] -> [Char]
readId (c:cs)
  | (isLower c) == True = c:readTypeJgt cs
  | otherwise = "Error"

-- | Check there is a colon between id and type.
readTypeJgt :: [Char] -> [Char]
readTypeJgt (c:cs)
  | c == ':' = c:readType cs
  | otherwise = "Error"

-- | Check the type is a known type.
readType :: [Char] -> [Char]
readType s
  | take 4 s == "Bool" = "Bool"
  | take 3 s == "Nat" = "Nat"
  | take 3 s == "Str" = "Str"
  | take 4 s == "Pair" = "Pair"
  | otherwise = "Error"

--}
