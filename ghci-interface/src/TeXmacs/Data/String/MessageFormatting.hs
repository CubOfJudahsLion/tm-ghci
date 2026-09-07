{- |
    Module      : TeXmacs.Data.String.MessageFormatting
    Description : Utilities to format a message to be sent to TeXmacs
    Copyright   : (c) Alexander Feterman Naranjo, 2023-26
    License     : GPL-3
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Data meant for TeXmacs needs to be bracketed by specials symbols
    (see 'TeXmacs.Data.Char.ControlCharacters') and formatted in certain
    ways.
-}

module TeXmacs.Data.String.MessageFormatting ( formatForTeXmacs ) where

import Data.List ( unsnoc, singleton )
import Data.Tuple ( swap )
import TeXmacs.Data.Char.ControlCharacters
import System.Process.CommunicationHandle (createTheyReadWeWritePipe)


--  Escapes any TeXmacs special characters in a string.
escapeForTeXmacs :: String -> String
escapeForTeXmacs = concatMap escapeChar
  where
    escapeChar :: Char -> String
    escapeChar ch
      | ch `elem` [dataEscape, dataBegin, dataEnd]  = [dataEscape, ch]
      | otherwise                                   = singleton ch


-- |  Edits and formats a 'String' to match TeXmacs's simple I/O protocol.
formatForTeXmacs :: Bool -> String -> String
formatForTeXmacs withPrompt message =
  if withPrompt then
    --  Render all lines as verbatim, except for the
    --  last one which renders as a prompt
    let msgLines          = lines message
        promptAndResponse = fmap unlines . swap <$> unsnoc msgLines
    in case promptAndResponse of
      Just (prompt, response) ->
        dataBegin : "verbatim:" ++
          (if null response
            then "\n"
            else escapeForTeXmacs response) ++
        dataEnd :
        dataBegin : "prompt#" ++ escapeForTeXmacs prompt ++ dataEnd : ""
      _                       ->
        dataBegin : "prompt#ghci>" ++ dataEnd : ""
  else
    --  Render all as verbatim
    dataBegin : "verbatim:" ++ escapeForTeXmacs message ++ dataEnd : ""

