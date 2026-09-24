{- |
    Module      : TmGHCi.Control.IO.Bridging
    Description : Input\/Output control between /GHCi/ and /TeXmacs/
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Messages might contain control requests or require special formatting,
    stream sets for each application follow different conventions, etc. Any
    such concerns are handled by the /I\/O loop function/.
-}

{-# LANGUAGE PatternSynonyms #-}

module TmGHCi.Control.IO.Bridging ( mainLoop ) where


import Control.Arrow ( Arrow(first, (***)), (>>>) )
import Control.Concurrent ( threadDelay )
import Control.Monad ( when )
import Data.List ( intercalate, singleton )
import Data.List.NonEmpty ( toList )
import Data.Maybe ( maybe )
import GHCi.Control.IO.Types  ( OutputTag(..), Tagged, pattern (:@)
                              , TaggedLine, TaggedLines1
                              , GHCiHandles(..)
                              )
import GHCi.Control.IO.Output.Sequencing
import GHCi.Data.String.Utils
import System.IO  ( Handle
                  , stdin, stdout
                  -- , hSetEncoding, utf16le
                  , hPutStr, hFlush
                  )
import System.IO.Strict ( readImmediate )
import TeXmacs.Data.String.Formatting


------------------------------------------
--  Utility functions
------------------------------------------

--  Convert a 'OutputTag' into its corresponding 'FormatAs' (for use
--  with 'formatForTeXmacs'.)
tagToFormat :: OutputTag -> FormatAs
tagToFormat Err = AsError
tagToFormat Out = AsOutput


--  Writes a 'TaggedLine', using the proper format and output stream.
formatTaggedLine :: TaggedLine -> String
formatTaggedLine (tag :@ plainText) = formatForTeXmacs (tagToFormat tag) plainText


--  Turns outputs and prompt into a single formatted line.
joinLinesAndPrompt :: (Maybe TaggedLines1, Maybe String) -> String
joinLinesAndPrompt  =       maybe [] (toList >>> fmap formatTaggedLine)
                        *** maybe [] (formatForTeXmacs AsPrompt >>> singleton)
                    >>> uncurry (++)
                    >>> intercalate "\n"

------------------------------------------
--  Main I/O loop
------------------------------------------

-- |  I/O loop function. Handles message traffic between the two
--    programs intelligently, taking care of conversions and events.
mainLoop :: GHCiHandles -> IO ()
mainLoop (GHCiHandles {ghciIn, ghciOut, ghciErr}) = do
  --hSetEncoding stdout utf16le
  loop
  where
    loop :: IO ()
    loop  =   captureOutputs (Out :@ ghciOut, Err :@ ghciErr)
          >>= hPutStr stdout . joinLinesAndPrompt . extractPrompt
          >>  hFlush stdout
          >>  readImmediate stdin
          >>= hPutStr ghciIn . censorQuitCommand . toList
          >>  hFlush ghciIn
          >>  loop

