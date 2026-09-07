{- |
    Module      : TeXmacs.Data.String.Utils.Parsers
    Description : Parser routines for message processing
    Copyright   : (c) Alexander Feterman Naranjo, 2023-26
    License     : GPL-3
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    These routines allow recognition of special messages and/or data
    extraction from them.
-}

module TeXmacs.Data.String.Utils.Parsers where

import Data.Char ( isSpace )
import Data.List ( uncons )
import Data.Maybe ( fromMaybe )
import Text.ParserCombinators.ReadP


------------------------------------------
--  Parser result retrievers
------------------------------------------

-- |  Selects the first parsed result after running a parser, if any.
firstParse :: ReadP a -> String -> Maybe a
firstParse parser = fmap (fst . fst) . uncons . readP_to_S parser


-- |  Selects the first parsed result, with a default if the parser fails.
firstParseOr :: ReadP a -> a -> String -> a
firstParseOr parser def = fromMaybe def . firstParse parser


------------------------------------------
--  Parsers
------------------------------------------

-- |  Recognizes the :quit command. 
quitCommandParser :: ReadP Bool
quitCommandParser =   skipSpaces
                  *>  (   True
                      <$  (   (   -- Single or double colons are accepted as command starters
                                  char ':' <* optional (char ':')
                                  -- Only the first letter is mandatory
                              <*  char 'q'
                              <*  optional (char 'u'
                              <*  optional (char 'i'
                              <*  optional (char 't')))
                              )
                              -- The command must be followed by a space (the rest doesn't matter) or end there
                          <*  choice [eof, () <$ satisfy isSpace]
                          )
                      )

