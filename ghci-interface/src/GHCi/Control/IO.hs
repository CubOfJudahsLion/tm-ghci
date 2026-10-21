{- |
    Module      : GHCi.Control.IO
    Description : I/O operations and obstractions over /GHCi./
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Provides 'Streams' corresponding to /GHCi/'s standard handles and
    corresponding operations, plus some extra facilities.
-}

{-# LANGUAGE DuplicateRecordFields #-}

module GHCi.Control.IO
  ( GHCiHandles(..)
  , GHCiStreams(..)
  , module GHCi.Control.IO.Types
  , module GHCi.Control.IO.Reading
  , module GHCi.Control.IO.Sequencing
  ) where


import GHCi.Control.IO.Types
import GHCi.Control.IO.Output.Reading
import GHCi.Control.IO.Output.Sequencing

import System.IO ( Handle )


-- |  Holds a set of standard handles for /GHCi/. Named
--    fields make it harder to commit ordering errors.
data GHCiHandles = MkGHCiHandles
  { ghciIn  :: !Handle
  , ghciOut :: !Handle
  , ghciErr :: !Handle
  }


data GHCiStreams = MkGHCiStreams
  { ghciIn  :: !Handle
  , ghciOut :: !Handle
  , ghciErr :: !Handle
  }

