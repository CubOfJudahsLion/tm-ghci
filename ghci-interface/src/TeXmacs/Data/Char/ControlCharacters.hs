{- |
    Module      : TeXmacs.Data.CharData.ControlCharacters
    Description : Characters with special meanings for /TeXmacs/
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Characters with special meanings for /TeXmacs/.
-}

module TeXmacs.Data.Char.ControlCharacters where


-- |  (@\\^B@) __B__egins a message
dataBegin :: Char
dataBegin   = '\02'

-- |  (@\\^E@) __E__nds a message
dataEnd :: Char
dataEnd     = '\05'

-- |  'dataEnd' as a single-character string, as it's
--    often the last element in a string construction.
dataEndStr :: String
dataEndStr = [dataEnd]


-- |  (@\\^[@ or @\\ESC@) __Esc__apes a succeeding 'dataBegin' or 'dataEnd' (or itself)
dataEscape :: Char
dataEscape  = '\27'

