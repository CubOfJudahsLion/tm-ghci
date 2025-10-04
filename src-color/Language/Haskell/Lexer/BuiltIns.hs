{-|
Module      : Language.Haskell.Lexer.BuiltIns
Description : Lists of lexemes grouped by type
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : GPL-3
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX
-}


module Language.Haskell.LexemeTypes
  ( LexemeType
  , keywords
  , languagePunctuation
  )
  where


import Data.List ( sort, uncons )
import Data.Vector.Strict ( Vector, unfoldr )


----------------------------------------
-- Lists used for construction
----------------------------------------

--  List of keywords, pre-sorted.
keywordsList :: [String]
keywordsList = sort [ "_"           -- Unbound pattern match
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
                    , "proc"
                    , "qualified"
                    , "rec"
                    , "then"
                    , "type"
                    , "where"
                    ]

--  Lists symbols that form part of the language, pre-sorted.
languagePunctuationList :: [String]
languagePunctuationList = sort [ "!"        -- Bang pattern
                               , "#"        -- Unboxed types
                               , "("        -- Start subexpression
                               , ")"        -- End subexpression
                               , ","        -- Open parens
                               , "->"       -- Returns type
                               , "::"       -- Has type
                               , ";"        -- Separator inside braces
                               , "<-"       -- do-notation binding
                               , "=>"       -- Constraint application
                               , "?"        -- Type hole
                               , "@"        -- Whole-to-parts pattern matching, type application
                               , "["        -- Start list
                               , "[|"       -- Quotation start
                               , "]"        -- End list
                               , "{"        -- Free layout starter
                               , "|"        -- Alternative separator
                               , "|]"       -- Quotation end
                               , "}"        -- Free layout ender
                               , "~"        -- Type equality, irrefutable pattern matching
                               ]


----------------------------------------
--  Final-form vectors. Strict and
--  ready for binary search.
----------------------------------------

-- |  Vector containing all Haskell keywords, pre-sorted.
keywords :: Vector String
keywords = unfoldr uncons keywordsList

-- | Vector with symbols that form part of the language, pre-sorted. These can't be used as operators.
languagePunctuation :: Vector String
languagePunctuation = unfoldr uncons languagePunctuationList

