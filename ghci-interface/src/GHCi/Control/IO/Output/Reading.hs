{-
    Module      : GHi.Control.IO.Output.Reading
    Description : Low-level routines reading /GHCi/ output
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX
-}

{-# LANGUAGE PatternSynonyms #-}

module GHCi.Control.IO.Output.Reading ( readAndTagAvailable ) where


import Control.Arrow ( Arrow((&&&)), (>>>) )
import Control.DeepSeq ( (<$!!>) )
import Data.List.NonEmpty ( NonEmpty(..), toList, nonEmpty )
import Data.Maybe ( fromMaybe )
import GHCi.Control.IO.Types ( TaggedLines1, OutputTag, pattern (:@) )
import System.IO ( Handle )
import System.IO.StrictImmediate


--  Reads text using 'readAvailable', splitting it into lines and tagging them with the
--  provided 'OutputTag'.
splitAndTag :: OutputTag -> NonEmpty Char -> TaggedLines1
splitAndTag tag =   toList
                >>> (:| []) &&& (lines >>> nonEmpty)
                >>> uncurry fromMaybe
                >>> fmap (tag :@)


-- |  Reads all immediately available characters (using 'readAvailable') and then splits
--    the result into lines and applies the tag to each (using 'splitAndTag'.)
readAndTagAvailable :: OutputTag -> Handle -> IO TaggedLines1
readAndTagAvailable tag = (splitAndTag tag <$!!>) . readAvailable

