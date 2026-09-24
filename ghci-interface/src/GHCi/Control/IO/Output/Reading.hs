{- |
    Module      : GHi.Control.IO.Output.Reading
    Description : Low-level routines for reading /GHCi/ output
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Low-level routines for reading /GHCi/ output.
-}

{-# LANGUAGE PatternSynonyms #-}

module GHCi.Control.IO.Output.Reading ( readAndTagAvailable ) where


import Control.Arrow ( Arrow((&&&)), (>>>) )
import Control.DeepSeq ( (<$!!>) )
import Data.List.NonEmpty ( NonEmpty(..), toList, nonEmpty )
import Data.Maybe ( fromMaybe )
import GHCi.Control.IO.Types ( TaggedLines1, OutputTag, pattern (:@) )
import System.IO ( Handle )
import System.IO.Strict ( readAvailable )


--  Reads text using 'readAvailable', splitting it into lines and tagging them with the
--  provided 'OutputTag'.
splitAndTag :: OutputTag      -- ^  Tag to attach
            -> NonEmpty Char  -- ^  Text to split
            -> TaggedLines1   -- ^  Returns the list of tagged lines
splitAndTag tag =   toList
                >>> (:| []) &&& (lines >>> nonEmpty)
                >>> uncurry fromMaybe
                >>> fmap (tag :@)


-- |  Reads all [near-]immediately available characters  and then splits the result in
--    o lines and applies the tag to each. This function uses 'readAvailable'; see it
--    for further details on parameters and behavior.
readAndTagAvailable :: OutputTag        -- ^  'OutputTag' to attach to resulting lines
                    -> Int              -- ^  Grace period between character reads
                    -> Handle           -- ^  'Handle' to reag from
                    -> Handle           -- ^  Alternate 'Handle' to check
                    -> IO TaggedLines1  -- ^  Returns a 'NonEmpty' list of tagged lines.
readAndTagAvailable !tag !wait !handle !otherHandle = (splitAndTag tag <$!!>) $ readAvailable wait handle (Just otherHandle)

