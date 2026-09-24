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


--  Turns any special character into an escaped pair, anything
--  else into a single-charcter string.
escapeCharForVerbatim :: Char -> String
escapeCharForVerbatim ch
  | ch `elem` [dataEscape, dataBegin, dataEnd]  = [dataEscape, ch]
  | otherwise                                   = singleton ch


-- |  Escapes any /TeXmacs/ special characters in a string for /@verbatim:@/ output
escapeForVerbatim :: String -> String
escapeForVerbatim = concatMap escapeCharForVerbatim


-- |  Escapes characters with special meanings in /LaTeX/ in string meant
--    for /@latex:@/ output.
escapeForLaTeX :: String -> String
escapeForLaTeX = concatMap escapeCharForLaTeX
  where
    --  Escapes both special /TeXmacs/ and /LaTeX/ special characters.
    escapeCharForLaTeX :: Char -> String
    escapeCharForLaTeX '\\'                                           = "{\\textbackslash}"
    escapeCharForLaTeX '~'                                            = "\\~{}"
    escapeCharForLaTeX c | c `elem` ['{', '}', '&', '$', '%', '#']    = "{\\" ++ c : "}"
                         | otherwise                                  = escapeCharForVerbatim c

