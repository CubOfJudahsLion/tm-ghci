{- |
    Module      : TeXmacs.Data.String.Escaping
    Description : String escaping for various TeXmacs formats
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

-   /TeXmacs/'s internal message formats (@verbatim@ and @scheme@)
    require transforming strings meant as literals to avoid accidental
    control messages.
-}

module TeXmacs.Data.String.Escaping where


import Data.List ( singleton )
import TeXmacs.Data.Char.ControlCharacters
import Text.Printf ( printf )


-- |  Escapes any /TeXmacs/ special characters in a string for /verbatim/ output
escapeForVerbatim :: String -> String
escapeForVerbatim = concatMap escapeChar
  where
    --  Turns any special character into an escaped pair, anything
    --  else into a single-charcter string.
    escapeChar :: Char -> String
    escapeChar ch
      | ch `elem` [dataEscape, dataBegin, dataEnd]  = [dataEscape, ch]
      | otherwise                                   = singleton ch


-- |  Escapes double quotes, control characters and backslashes in a message
--    meant for /scheme/ output.
escapeForScheme :: String -> String
escapeForScheme = concatMap escapeChar
  where
    escapeChar :: Char -> String
    escapeChar '\\'           = "\\\\"
    escapeChar '"'            = "\\\""
    escapeChar c | c < '\32'  = printf "\\x%02hhd" c
                 | otherwise  = singleton c

