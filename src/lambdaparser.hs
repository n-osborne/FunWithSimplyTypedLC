{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module : LambdaParser
Copyrigth : Nicolas Osborne, 2018
Licence : MIT

Provide the function pg2ast which turn a program written in Simply Typed Lambda
Calculus into an Abstract Syntax Tree.
-}
module LambdaParser
  ( pg2ast ) where

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
