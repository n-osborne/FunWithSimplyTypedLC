{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module : Ast2Tree
Copyright : (c) 2018 Nicolas Osborne
Licence : BSD 3-clauses

Provide the function to turn an Abstract Syntax Tree into a String coding the
corresponding tree for the @dot@ unix program.
-}
module Ast2Tree
  ( ast2tree )
  where

import Lambda2Ast
import Utils

-- * Exported function

-- | Return the string describing in @dot@ language the tree corresponding to
-- the given AST.
ast2tree :: AST -> [Char]
ast2tree ast = "digraph G {\n" ++ (ast2tree' 0 ast) ++ "}\n"

-- * Helper functions

-- | Return the internal description of the AST.
ast2tree' :: Int -> AST -> [Char]
ast2tree' n ast
  | elem (name ast) binop = binary2tree n ast
  | elem (name ast) unop = unary2tree n ast
  | otherwise = leaf2tree n ast
  where binop = ["lambda", "apply", "pi"]
        unop = ["first", "second", "succ"]

-- | Return the part of the string describing a binary node of an AST in the
-- @dot@ language.
binary2tree :: Int -> AST -> [Char]
binary2tree n ast = let sizeLeft = size (left ast)
                        toLeft = "\t" ++ (show n) ++ " -> " ++ (show (n+1)) ++ ";\n"
                        toRight = "\t" ++ (show n) ++ " ->" ++ (show (n + 1 + sizeLeft)) ++ ";\n"
                        node = printNode n ast
                        leftNode = ast2tree' (n+1) (left ast)
                        rightNode = ast2tree' (n + 1 + sizeLeft) (right ast)
                    in toLeft ++ toRight ++ node ++ leftNode ++ rightNode

-- | Return the part of the string describing an unary node of an AST in the
-- @dot@ language.
unary2tree :: Int -> AST -> [Char]
unary2tree n ast =  toNext ++ node ++ next
  where toNext = "\t" ++ (show n) ++ " -> " ++ (show (n+1)) ++ ";\n"
        node = printNode n ast
        next = ast2tree' (n+1) (below ast)

-- | Return the part of the string describing a leaf of an AST in the @dot@ language.
leaf2tree :: Int -> AST -> [Char]
leaf2tree n ast = printNode n ast

-- | Return the part of the string describing the node label in the description
-- of the given 'AST' in the @dot@ language
printNode :: Int -> AST -> [Char]
printNode n ast = "\t" ++ (show n) ++ " [label=\"" ++  (name ast) ++ " : " ++ (ltype ast) ++ "\"];\n"
