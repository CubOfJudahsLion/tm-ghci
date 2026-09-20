{- |
    Module      : TeXmacs.Data.String.Formatting
    Description : Utilities to format a message to be sent to /TeXmacs/
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Data meant for /TeXmacs/ needs to be bracketed by specials symbols
    (see "TeXmacs.Data.Char.ControlCharacters") and formatted in certain
    ways. This module provides the formatting function.
-}


{-# LANGUAGE CPP #-}

module TeXmacs.Data.String.Formatting ( FormatAs(..), formatForTeXmacs ) where


import TeXmacs.Data.Char.ControlCharacters ( dataBegin, dataEnd, dataEndStr )
import TeXmacs.Data.String.Escaping


-- |  States the intended purpose (and consequent formatting) of the text to
--    be converted.
data FormatAs = AsOutput  -- for @STDOUT@
              | AsError   -- for @STDERR@
              | AsPrompt  -- for the "@prompt@" channel


--  The @CONSOLE_DEBUG@ flag is used when testing in such way. Mostly implies
--  coloring the output according to the text's purpose. Errors are colored
--  red and prompts blue. Normal text is not enhanced.
#if CONSOLE_DEBUG
ansiStart, ansiRed, ansiReset, ansiEnd :: String
ansiStart = "\ESC[" --  starts an ANSI code sequence
ansiEnd = "m"       --  end the ANSI code sequence
ansiRed = "31"      --  sets foreground text color to red
ansiBlue = "34"     --  sets foreground text color to blue
ansiReset = "0"     --  resets all color and formatting to default
#endif


-- |  Formats a string to be fit for output to /TeXmacs/
--    depending on its intended function..
formatForTeXmacs :: FormatAs -> String -> String
--
formatForTeXmacs AsPrompt text =
  dataBegin :
#if CONSOLE_DEBUG
    ansiStart ++ ansiBlue ++ ansiEnd ++
#endif
      "prompt#" ++
      escapeForVerbatim text ++
#if CONSOLE_DEBUG
    ansiStart ++ ansiReset ++ ansiEnd ++
#endif
  dataEndStr
--
formatForTeXmacs AsError text =
  dataBegin :
#if CONSOLE_DEBUG
    ansiStart ++ ansiRed ++ ansiEnd ++
#endif
      --"scheme:(document (with \"color\" \"red\" \"" ++
      --  escapeForScheme text ++
      --"\"))" ++
      "latex:\\tmcolor{red}{" ++
        escapeForLaTeX text ++
      '}' :
#if CONSOLE_DEBUG
    ansiStart ++ ansiReset ++ ansiEnd ++
  dataEnd : "\n"
#else
  dataEndStr
#endif
--
formatForTeXmacs AsOutput text =
  dataBegin :
    "verbatim:" ++
    escapeForVerbatim text ++
#if CONSOLE_DEBUG
  dataEnd : "\n"
#else
  dataEndStr
#endif

