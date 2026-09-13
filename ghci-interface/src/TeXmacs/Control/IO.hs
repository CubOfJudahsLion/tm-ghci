{- |
    Module      : TeXmacs.Control.IO
    Description : Input/Output control between /GHCi/ and /TeXmacs/
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Messages might contain control requests and/or require
    special formatting. Any such concerns are also handled by
    the /I\/O loop function/.
-}

module TeXmacs.Control.IO ( mainLoop ) where


import Control.Arrow ( Arrow(first), (>>>) )
import Control.Concurrent ( threadDelay )
import Control.Monad ( when )
import Data.List.NonEmpty ( toList )
import GHCi.Control.IO.Types
import GHCi.Control.IO.Output.Processing
import System.IO  ( Handle
                  , stdin, stdout
                  , hReady
                  , hGetChar, hPutStr, hFlush
                  )
import TeXmacs.Data.String.MessageFormatting
import TeXmacs.Data.String.Utils


------------------------------------------
--  Utility functions
------------------------------------------

--  Convert a 'GHCi.Control.IO.Types.OutputTag' into its corresponding
--  'TeXmacs.Data.String.MessageFormatting.FormatAs' (for use with
--  'TeXmacs.Data.String.MessageFormatting.format')
tagToFormat :: OutputTag -> FormatAs
tagToFormat Err = AsError
tagToFormat Out = AsOutput


------------------------------------------
--  Main I/O loop
------------------------------------------

-- |  Handles message traffic between the two programs,
--    including conversions and events.
mainLoop :: GHCiHandles -> IO ()
mainLoop (GHCiHandles {ghciIn, ghciOut, ghciErr}) = loop
  where
    --  Writes a 'TaggedLine', using the proper format and output stream
    writeTaggedLine :: TaggedLine -> IO ()
    writeTaggedLine =   first tagToFormat
                    >>> uncurry formatForTeXmacs
                    >>> hPutStr stdout
                    >>> (>> hFlush stdout)
    --  Loop worker
    loop :: IO ()
    loop = do
      (maybePrompt, maybeLines) <-  fmap joinEqualOutputs
                                <$> (extractPrompt
                                <$> captureOutputs (Out :@ ghciOut, Err :@ ghciErr))
      case maybeLines of
        Just lines  ->  mapM_ writeTaggedLine lines
        Nothing     ->  pure ()
      case maybePrompt of
        Just prompt ->  do  hPutStr stdout (formatForTeXmacs AsPrompt prompt)
                            hFlush stdout
        Nothing     ->  pure ()
      readAvailable stdin >>= (toList >>> censorQuitCommand >>> hPutStr ghciIn)
                          >>  hFlush ghciIn
      loop

