{- |
    Module      : TeXmacs.Data.CharData.ControlCharacters
    Description : Characters with special meanings for /TeXmacs/
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX
-}

module TeXmacs.Data.Char.ControlCharacters where


--  | Escape characters
dataBegin, dataEnd, dataEscape :: Char
dataBegin   = '\02'    -- ^  (aka @\\STX@ or @\\^B@) __B__egins a character sequence.
dataEnd     = '\05'    -- ^  (aka @\\ENQ@ or @\\^E@) __E__nds a character sequence.
dataEscape  = '\27'    -- ^  (aka @\\ESC@ or @\\^[@) Used to __Esc__ape 'dataBegin' or 'dataEnd' (or itself) if found inside a message.

