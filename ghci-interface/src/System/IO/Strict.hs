{- |
    Module      : System.IO.Strict
    Description : Strict processing of [near-]immediately available stream data
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Strict processing of immediately available stream data.
-}


module System.IO.Strict where


import Control.DeepSeq ( ($!!) )
import Data.List.NonEmpty ( NonEmpty(..) )
import System.IO ( Handle, hGetChar, hReady, hWaitForInput )


-- |  Reads all available characters in a __strict__ manner, either synchronously or
--    given a grace idle period. The idle period is skipped if 'otherHandle' is
--    available for reading. The funcion is strict on its arguments.
readAvailable :: Int                  -- ^  Grace period between character reads.
                                      --    __0__ means /"read only immediately available."/
              -> Handle               -- ^  'Handle' to read from.
              -> Handle               -- ^  Alternate 'Handle' to check. If the read handle isn't ready but this one is,
                                      --    no wait time is conceded and the function returns.
              -> IO (NonEmpty Char)   -- ^  Returns at least one read character.
readAvailable !wait !handle !otherHandle = (:|) <$> hGetChar handle >>= worker
  where
    worker  :: ([Char] -> NonEmpty Char)  --  Accumulator as difference list
            -> IO (NonEmpty Char)         --  Returns at least one read character (i.e., at least that in the diff. list.)
    worker !accum = do
      --  Read only immediately-available characters. Any time gap might
      --  represent output has ended or switched to another stream
      otherReady <- hReady otherHandle
      ready <-  if wait == 0 || otherReady then
                  hReady handle
                else
                  hWaitForInput handle wait
      if not ready then
        pure $!! accum ""
      else do
        !ch <- hGetChar handle
        let accum' = accum . (ch :)
        worker accum'


-- |  Reads all immediately available characters (i.e., synchronously) in a __strict__
--    manner. @readImmediate h@ is a shorthand for @'readAvailable' 0 h h@.
readImmediate :: Handle -> IO (NonEmpty Char)
readImmediate h = readAvailable 0 h h
{-# INLINEABLE readImmediate #-}

