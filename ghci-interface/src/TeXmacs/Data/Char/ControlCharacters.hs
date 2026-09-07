{- |
    Module      : TeXmacs.Data.CharData.ControlCharacters
    Description : Characters that mark message boundaries or escape the former
    Copyright   : (c) Alexander Feterman Naranjo, 2023-26
    License     : GPL-3
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Characters with special meanings for TeXmacs.
-}

module TeXmacs.Data.Char.ControlCharacters where


--  | Escape characters
dataBegin, dataEnd, dataEscape :: Char
dataBegin   = '\x02'    -- ^  (aka @\\STX@, @\\^B@) __B__egins a character sequence.
dataEnd     = '\x05'    -- ^  (aka @\\ENQ@, @\\^E@) __E__nds a character sequence.
dataEscape  = '\x1B'    -- ^  (aka @\\ESC@, @\\^[@) Used to __E__scape 'dataBegin' or 'dataEnd' (or itself) if found inside a message.
