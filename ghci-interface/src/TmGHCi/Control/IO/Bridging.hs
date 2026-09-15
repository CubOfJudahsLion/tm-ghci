{- |
    Module      : TmGHCi.Control.IO.Bridging
    Description : Input\/Output control between /GHCi/ and /TeXmacs/
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Messages might contain control requests or require special formatting,
    output streams follow different ordering rules, etc. Any such concerns
    are handled by the /I\/O loop function/.
-}

{-# LANGUAGE PatternSynonyms #-}

module TmGHCi.Control.IO.Bridging ( mainLoop ) where


import Control.Concurrent ( threadDelay )
import Control.Monad ( when )
import Data.List.NonEmpty ( toList )
import GHCi.Control.IO.Types  ( OutputTag(..)
                              , pattern (:@)
                              , TaggedLine
                              , GHCiHandles(..)
                              )
import GHCi.Control.IO.Output.Sequencing
import GHCi.Data.String.Utils
import System.IO  ( Handle
                  , stdin, stdout
                  , hReady
                  , hGetChar
                  , hPutStr, hFlush
                  )
import System.IO.StrictImmediate
import TeXmacs.Data.String.MessageFormatting


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
    writeTaggedLine (tag :@ plainText) = do
      let format        = tagToFormat tag
          formattedText = formatForTeXmacs format plainText
      hPutStr stdout formattedText
      hFlush stdout
    --
    --  Loop worker
    loop :: IO ()
    loop = do
      putStrLn "Starting loop, reading out/err"
      outs@(maybePrompt, maybeLines) <-  fmap joinEqualOutputs
                                <$> (extractPrompt
                                <$> captureOutputs (Out :@ ghciOut, Err :@ ghciErr))
      putStrLn "Done reading, result:"
      putStrLn $ show outs
      putStr "\n"
      case maybeLines of
        Just lines  ->  mapM_ writeTaggedLine lines
        Nothing     ->  pure ()
      case maybePrompt of
        Just prompt ->  do  hPutStr stdout (formatForTeXmacs AsPrompt prompt)
                            hFlush stdout
        Nothing     ->  pure ()
      readAvailable stdin >>= hPutStr ghciIn . censorQuitCommand . toList
                          >>  hFlush ghciIn
      putStrLn "Restarting loop"
      loop

