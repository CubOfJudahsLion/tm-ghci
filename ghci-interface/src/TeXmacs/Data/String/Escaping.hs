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
import TeXmacs.Data.Char.ControlCharacters ( dataBegin, dataEnd, dataEscape )


-- |  Escapes any /TeXmacs/ special characters in a string for /@verbatim:@/ output
escapeForVerbatim :: String -> String
escapeForVerbatim = concatMap escapeChar
  where
    --  Turns any special character into an escaped pair, anything
    --  else into a single-charcter string.
    escapeChar :: Char -> String
    escapeChar ch
      | ch `elem` [dataEscape, dataBegin, dataEnd]  = [dataEscape, ch]
      | otherwise                                   = singleton ch


-- |  Escapes characters with special meanings in /LaTeX/ in string meant
--    for /@latex:@/ output.
escapeForLaTeX :: String -> String
escapeForLaTeX = concatMap escapeChar
  where
    --  Escapes both special /TeXmacs/ and /LaTeX/ special characters.
    escapeChar :: Char -> String
    escapeChar '\\'                                           = "{\\textbackslash}"
    escapeChar '~'                                            = "\\~{}"
    escapeChar c | c `elem` ['{', '}', '&', '$', '%', '#']    = "{\\" ++ c : "}"
                 | c `elem` [dataEscape, dataBegin, dataEnd]  = [dataEscape, c]
                 | otherwise                                  = singleton c

