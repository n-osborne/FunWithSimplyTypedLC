{-# OPTIONS_HADDOCK ignore-exports #-}
{-|
Module : Ast2Tree
Copyright : (c) 2018 Nicolas Osborne
Licence : MIT

Provide the function to turn an Abstract Syntax Tree into a String coding the
corresponding tree for the @dot@ unix program.
-}
module Ast2Tree
  ()
  where

import Lambda2Ast
import Utils
