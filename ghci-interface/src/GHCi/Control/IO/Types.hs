{- |
    Module      : GHCi.Control.IO.Types
    Description : Data types for capturing and processing output from /GHCi/
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Data types for capturing and processing output from /GHCi/.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module GHCi.Control.IO.Types
  ( GHCiReadChannel(..)
  , GHCiWriteChannel(..)
  , SGHCiReadChannel(..)
  , SGHCiWriteChannel(..)
  , Tagged
  , pattern (:@)
  , TaggedLine
  , TaggedLines
  , TaggedLines1
  ) where


import Control.DeepSeq ( NFData(..) )
import Data.List.NonEmpty ( NonEmpty )
import Data.Kind ( Type )
import Data.Proxy ( Proxy(..) )
import TeXmacs.Data.String.Formatting ( FormatAs(AsOutput, AsError) )
import System.IO ( Handle )


-- |  Types of readable channels from /GHCi/.
type data GHCiReadChannel = GHCiOutputChannel
                          | GHCiErrorChannel

-- |  Singleton for 'GHCiReadChannel'.
data SGHCiReadChannel (channel :: GHCiReadChannel) where
  SGHCiOutputChannel  :: SGHCiReadChannel GHCiOutputChannel
  SGHCiErrorChannel   :: SGHCiReadChannel GHCiErrorChannel

deriving instance Eq (SGHCiReadChannel ch)

instance Show (SGHCiReadChannel GHCiOutputChannel) where
  show _ = "<GHCi standard output>"

instance Show (SGHCiReadChannel GHCiErrorChannel) where
  show _ = "<GHCi standard error>"

instance NFData (SGHCiReadChannel (channel :: GHCiReadChannel)) where
  rnf !_ = ()


-- |  Types of writable channels into /GHCi/.
type data GHCiWriteChannel = GHCiInputChannel

-- |  Singleton for 'GHCiWriteChannel'.
data SGHCiWriteChannel (channel :: GHCiWriteChannel) where
  SGHCiInputChannel :: SGHCiWriteChannel GHCiInputChannel

deriving instance Eq (SGHCiWriteChannel (channel :: GHCiWriteChannel))

instance Show (SGHCiWriteChannel (channel :: GHCiWriteChannel)) where
  show _ = "<GHCi standard input>"

-- |  A data to tag file output with its type of origin stream
-- |  Represents a datum tagged with a source stream
type Tagged (chan :: GHCiReadChannel) a = (SGHCiReadChannel chan, a)

-- |  @t :\@ a@ [de]constructs a @Tagged a@.
pattern (:@) :: (SGHCiReadChannel chan) -> a -> Tagged chan a
pattern t :@ a = (t, a)
infix 6 :@


-- |  Represents a single line tagged with its source stream. Note that we use a regular 'String'
--    (instead of a t'NonEmpty' 'Char') as /GHCi/ output can produce empty lines arbitrarily.
type TaggedLine (chan :: GHCiReadChannel) = Tagged chan String

-- |  A (possibly empty) list of 'TaggedLine's.
type TaggedLines (chan :: GHCiReadChannel) = [TaggedLine chan]

-- |  A 'NonEmpty' list of 'TaggedLine's.
type TaggedLines1 (chan :: GHCiReadChannel) = NonEmpty (TaggedLine chan)

