{-|
Module      : Language.Haskell.Lexer.BuiltIns
Description : Lists of lexemes grouped by type
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : GPL-3
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Lexer.BuiltIns
  ( keywords
  , operatorCharacters
  , punctuationCharacters
  , reservedOperators
  ) where


import Data.List ( sort, uncons )
import Data.Text ( Text )
import Data.Vector.Strict ( Vector, unfoldr )


----------------------------------------
-- Lists used for construction
----------------------------------------

--  The following are lists of string 

--  List of keywords, pre-sorted.
keywordsList :: [Text]
keywordsList = sort [ "_"           -- Pattern wildcard
                    , "anyclass"
                    , "as"
                    , "case"
                    , "class"
                    , "data"
                    , "default"
                    , "deriving"
                    , "do"
                    , "else"
                    , "family"
                    , "forall"
                    , "foreign"
                    , "hiding"
                    , "if"
                    , "in"
                    , "infix"
                    , "infixl"
                    , "infixr"
                    , "instance"
                    , "let"
                    , "mdo"
                    , "module"
                    , "newtype"
                    , "of"
                    , "pattern"
                    , "proc"
                    , "qualified"
                    , "rec"
                    , "static"
                    , "stock"
                    , "then"
                    , "type"
                    , "via"
                    , "where"
                    , "∀"         -- Unicode symbol for "forall"
                    ]

--  List of reserved operators, pre-sorted. No punctuation is figures here.
reservedOperatorsList :: [Text]
reservedOperatorsList = sort  [ "!"         -- Bang pattern (strict annotation)
                              , "#"         -- Unboxed type / Label
                              , "->"        -- Returns type / Case match result
                              , ".."        -- Range operator / All constructors (of a type in import lists)
                              , ":"         -- List head:rest pattern
                              , "::"        -- Has type
                              , "<-"        -- do-notation binding
                              , "="         -- Equational equality
                              , "=>"        -- Constraint application
                              , "?"         -- Type hole
                              , "@"         -- Whole-to-parts pattern matching / Type application
                              , "\\"        -- Lambda abstraction
                              , "|"         -- Data constructor separator / Guards / MultiWayIf alternatives / Functional dependencies / Unboxed sum
                              , "~"         -- Type equality / Lazy annotation
                              -- Unicode variants --
                              , "λ"         -- U03BB
                              , "←"         -- U2190
                              , "→"         -- U2192
                              , "⇒"         -- U21D2
                              , "∷"         -- U2237
                              ]


--  These are character classes as lists

--  Characters that can be used to construct operator names
operatorCharactersList :: [Char]
operatorCharactersList = sort ['!', '#', '$', '%', '&', '⋆', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~', ':']

--  Punctuation ("special") characters
punctuationCharactersList :: [Char]
punctuationCharactersList = sort ['(', ')', ',', ';', '[', ']', '`', '{', '}', '⦇', '⦈', '⟦', '⟧']


----------------------------------------
--  Final-form vectors. Strict and
--  ready for binary search.
----------------------------------------

-- |  'Vector' containing all /Haskell/ keywords, pre-sorted.
keywords :: Vector Text
keywords = unfoldr uncons keywordsList

-- |  Vector containing all the reserved operators, pre-sorted.
reservedOperators :: Vector Text
reservedOperators = unfoldr uncons reservedOperatorsList

-- |  Vector with ASCII characters that can be used in operator names.
operatorCharacters :: Vector Char
operatorCharacters = unfoldr uncons operatorCharactersList

-- |  Vector with characters (all Unicode) that can be used as puncuation.
punctuationCharacters :: Vector Char
punctuationCharacters = unfoldr uncons punctuationCharactersList

