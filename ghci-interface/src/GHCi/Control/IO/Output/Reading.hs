{- Module      : GHi.Control.IO.Output.Reading
    Description : Low-level routines reading /GHCi/ output
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX
-}

module GHCi.Control.IO.Output.Reading ( readAvailable, readAndTagAvailable ) where


import Control.Arrow ( Arrow((&&&)), (>>>) )
import Control.DeepSeq ( ($!!), (<$!!>) )
import Data.List.NonEmpty ( NonEmpty(..), toList, nonEmpty )
import Data.Maybe ( fromMaybe )
import GHCi.Control.IO.Types ( TaggedLines1, OutputTag, (:@) )
import System.IO ( Handle, hGetChar, hReady )


-- |  Reads all immediately available characters from 'Handle' @h@ in a /strict/ manner.
readAvailable :: Handle -> IO (NonEmpty Char)
readAvailable h = (:|) <$> hGetChar h >>= worker
  where
    worker :: ([Char] -> NonEmpty Char) -> IO (NonEmpty Char)
    worker !accum = do
      --  Read only immediately-available characters. Any time
      --  gap might represent output from another stream.
      !ready <- hReady h
      if not ready then
        pure $!! accum ""
      else do
        !ch <- hGetChar h
        let accum' = accum . (ch :)
        worker accum'


--  Reads text using 'readAvailable', splitting it into lines and tagging them with the
--  provided 'OutputTag'.
splitAndTag :: OutputTag -> NonEmpty Char -> TaggedLines1
splitAndTag tag =   toList
                >>> (:| []) &&& (lines >>> nonEmpty)
                    --  Even though the current implementation of 'lines' returns a non-empty
                    --  list of partitions for a non-empty string, 
                >>> uncurry fromMaybe
                >>> fmap (tag :@)


-- |  Reads all immediately available characters (using 'readAvailable') and then splits
--    the result into lines and applies the tag to each (using 'splitAndTag'.)
readAndTagAvailable :: OutputTag -> Handle -> IO TaggedLines1
readAndTagAvailable tag = (splitAndTag tag <$!!>) . readAvailable

