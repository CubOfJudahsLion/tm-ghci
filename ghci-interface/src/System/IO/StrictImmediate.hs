{- |
    Module      : System.IO.StrictImmediate
    Description : Strict processing of immediately available stream data
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Strict processing of immediately available stream data.
-}


module System.IO.StrictImmediate where


import Control.DeepSeq ( ($!!) )
import Data.List.NonEmpty ( NonEmpty(..) )
import System.IO ( Handle, hGetChar, hReady )


-- |  Reads all immediately available characters (i.e., everything that can
--    be obtained synchronously) from 'Handle' @h@ in a __strict__ manner.
--    It's also strict on its argument.
readAvailable :: Handle -> IO (NonEmpty Char)
readAvailable !h = (:|) <$> hGetChar h >>= worker
  where
    worker :: ([Char] -> NonEmpty Char) -> IO (NonEmpty Char)
    worker !accum = do
      --  Read only immediately-available characters. Any time gap might
      --  represent output has ended or switched to another stream
      !ready <- hReady h
      if not ready then
        pure $!! accum ""
      else do
        !ch <- hGetChar h
        let accum' = accum . (ch :)
        worker accum'

