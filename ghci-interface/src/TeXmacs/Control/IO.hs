{- c
    Module      : TeXmacs.Control.IO
    Description : Input/Output control between GHCi and TeXmacs
    Copyright   : (c) Alexander Feterman Naranjo, 2023-26
    License     : GPL-3
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Handles message traffic between the two programs. Since messages
    might contain data or simple control messages (or both), this
    requires some mediation.
-}

{-# LANGUAGE Arrows #-}

module TeXmacs.Control.IO ( GHCiHandles(..), mainLoop ) where


import System.IO  ( Handle
                  , stdin, stdout, stderr
                  , hWaitForInput
                  , hGetChar, hPutStr, hFlush
                  )
import TeXmacs.Data.String.MessageFormatting
import TeXmacs.Data.String.Utils


------------------------------------------
--  Data Types
------------------------------------------

-- |  Holds a set of standard handles for GHCi. The named fields
--    make it harder to commit ordering errors.
data GHCiHandles = GHCiHandles
  { ghciIn  :: !Handle
  , ghciOut :: !Handle
  , ghciErr :: !Handle
  }


------------------------------------------
--  Basic I/O Routines
------------------------------------------

--  Reads lines of input from 'Handle' @h@. We don't read line-wise
getInput :: Handle -> IO String
getInput h = checkWorker id
  where
    checkWorker :: (String -> String) -> IO String
    checkWorker !accum = do
      -- Char at this turn
      !ch <- hGetChar h
      -- Sometimes there are pauses in the input. Give it a little leeway.
      !ready <- hWaitForInput h 125
      let accum' = accum . (ch :)
      if not ready then
        pure $ accum' ""
      else
        checkWorker accum'


------------------------------------------
--  Main I/O loop
------------------------------------------

-- |  Main loop of GHCi interaction.
mainLoop :: GHCiHandles -> IO ()
mainLoop (GHCiHandles {ghciIn, ghciOut, ghciErr}) = do
  loop
  where
    loop = do
      getInput ghciOut >>= hPutStr stdout . formatForTeXmacs True >> hFlush stdout
      getInput stdin >>= hPutStr ghciIn . censorQuitCommand >> hFlush ghciIn
      loop

