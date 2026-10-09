{- |
    Module      : GHi.Control.IO.Input.Writing
    Description : Low-level routines for writing into /GHCi/
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Low-level routines for writng into /GHCi/'s intput.
-}

module GHCi.Control.IO.Input.Writing where

import System.IO ( Handle )

-- |  Stores a writable stream from a client process, i.e., one we can
--    write into. Likely to be a @stdin@ pipe.
newtype ClientWriter = MkClientWriter Handle

