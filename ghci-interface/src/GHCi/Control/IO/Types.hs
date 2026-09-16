{- |
    Module      : GHi.Control.IO.Types
    Description : Data types for capturing and processing output from /GHCi/
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Data types for capturing and processing output from /GHCi/.
-}

{-# LANGUAGE PatternSynonyms, DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module GHCi.Control.IO.Types
  ( GHCiHandles(..)
  , OutputTag(..)
  , Tagged
  , pattern (:@)
  , TaggedLine
  , TaggedLines
  , TaggedLines1
  ) where


import Control.DeepSeq ( NFData )
import Data.List.NonEmpty ( NonEmpty )
import GHC.Generics ( Generic )
import TeXmacs.Data.String.MessageFormatting ( FormatAs(AsOutput, AsError) )
import System.IO ( Handle )
import GHC.RTS.Flags (ProfFlags(descrSelector))


-- |  Holds a set of standard handles for /GHCi/. Named
--    fields make it harder to commit ordering errors.
data GHCiHandles = GHCiHandles
  { ghciIn  :: !Handle
  , ghciOut :: !Handle
  , ghciErr :: !Handle
  }


-- |  A data to tag file output with its type of origin stream
data OutputTag  = Err
                | Out
  deriving (Eq, Show, Generic, NFData)


-- |  Represents a datum tagged with a source stream
type Tagged a = (OutputTag, a)

-- |  @t :\@ a@ is a synonym for @(t, a)@. Useful to have in case the representation changes.
pattern (:@) :: OutputTag -> a -> Tagged a
pattern t :@ a = (t, a)
infix 6 :@


-- |  Represents a single line tagged with its source stream. Note that we use a regular
--    'String' (instead of a t'NonEmpty' 'Char') as output can produce empty lines arbitrarily.
type TaggedLine = Tagged String

-- |  A list (possibly empty) of 'TaggedLine' tuples
type TaggedLines = [TaggedLine]

-- |  A 'NonEmpty' list of 'TaggedLine' tuples
type TaggedLines1 = NonEmpty TaggedLine

