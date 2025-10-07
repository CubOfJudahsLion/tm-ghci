{-|
Module      : Data.Vector.Utils.BinarySearch
Description : Binary search in strict 'Vector's.
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : GPL-3
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE BangPatterns #-}
module Data.Vector.Utils.BinarySearch where


import Data.Vector.Strict ( (!), Vector )
import qualified Data.Vector.Strict as V ( length )


-- |  Performs a binary search within the index range. 'Vector' is assumed to be pre-sorted.
--    Both indexes must be inclusive and the pair must be ordered.
binarySearchInRange :: Ord a => (Int, Int) -> Vector a -> a -> Maybe Int
binarySearchInRange (start, end) vec value = searchWorker (max start 0, min end $ V.length vec - 1)
  where
    searchWorker :: (Int, Int) -> Maybe Int
    searchWorker (!left, !right) =
      if left > right then
        Nothing
      else
        let middle = (left + right) `div` 2
        in  if value == (vec ! middle) then
              Just middle
            else
              let searchRange = if value < (vec ! middle) then
                                  (left, middle - 1)
                                else
                                  (middle + 1, right)
              in  searchWorker searchRange

-- |  Performs a binary search an the entire 'Vector', which must be pre-sorted.
binarySearch :: Ord a => Vector a -> a -> Maybe Int
binarySearch vec = binarySearchInRange (0, V.length vec - 1) vec

