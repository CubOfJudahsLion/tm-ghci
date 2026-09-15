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

module TeXmacs.Data.Char.ControlCharacters
  ( -- |  (aka /@\\STX@/ or /@\\^B@/) __B__egins a character sequence.
    dataBegin
    -- |  (aka /@\\ENQ@/ or /@\\^E@/) __E__nds a character sequence.
  , dataEnd
   -- |  (aka /@\\ESC@/ or /@\\^[@/) Used to __Esc__ape 'dataBegin' or 'dataEnd' (or itself) if found inside a message.
  , dataEscape
  ) where
import GHC.RTS.Flags (ProfFlags(descrSelector))


--  | Escape characters
dataBegin, dataEnd, dataEscape :: Char
dataBegin   = '\02'
dataEnd     = '\05'
dataEscape  = '\27'

