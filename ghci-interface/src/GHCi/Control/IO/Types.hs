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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module GHCi.Control.IO.Types
  ( -- *  Reader channels
    GHCiReadChannel(..)
  , SingGHCiReadChannel(..)
  , WrapGHCiReadChannel(..)
    -- ** Channel-tagging types and ptterns
  , Tagged
  , pattern (:<>)
  , TaggedLine
  , TaggedLines
  , TaggedLines1
    -- *  Writer channels
  , GHCiWriteChannel(..)
  , SingGHCiWriteChannel(..)
  ) where


import Control.DeepSeq ( NFData(rnf) )
import Data.Kind ( Type, Constraint )
import Data.List.NonEmpty ( NonEmpty )


--------------------------------------------------------------------------------
--  Reader channels and related types
--------------------------------------------------------------------------------

-- |  Types of readable channels from /GHCi/.
type data GHCiReadChannel = GHCiOutputChannel
                          | GHCiErrorChannel

-- |  Singleton for 'GHCiReadChannel'.
data SingGHCiReadChannel (channel :: GHCiReadChannel) where
  SingGHCiOutputChannel  :: SingGHCiReadChannel GHCiOutputChannel
  SingGHCiErrorChannel   :: SingGHCiReadChannel GHCiErrorChannel

instance NFData (SingGHCiReadChannel (channel :: GHCiReadChannel)) where
  rnf !_ = ()

-- |  Existential wrapper around 'SingGHCiReadChannel'.
data WrapGHCiReadChannel where
  MkWrapGHCiReadChannel :: forall (channel :: GHCiReadChannel). SingGHCiReadChannel channel -> WrapGHCiReadChannel

instance Show WrapGHCiReadChannel where
  show :: WrapGHCiReadChannel -> String
  show (MkWrapGHCiReadChannel SingGHCiOutputChannel) = "<GHCi stdout>"
  show (MkWrapGHCiReadChannel SingGHCiErrorChannel)  = "<GHCi stderr>"

instance Eq WrapGHCiReadChannel where
  (==) (MkWrapGHCiReadChannel SingGHCiOutputChannel) (MkWrapGHCiReadChannel SingGHCiOutputChannel) = True
  (==) (MkWrapGHCiReadChannel SingGHCiErrorChannel)  (MkWrapGHCiReadChannel SingGHCiErrorChannel)  = True
  (==) _                                             _                                             = False

instance NFData WrapGHCiReadChannel where
  rnf !(MkWrapGHCiReadChannel singleton) = rnf singleton


--------------------------------------------------------------------------------
--  Channel-tagging types
--------------------------------------------------------------------------------

-- |  A data to tag file output with its type of origin stream
-- |  Represents a datum tagged with a source stream
type Tagged a = (WrapGHCiReadChannel, a)

-- |  @w :<> a@ [de]constructs a 'Tagged a'.
pattern (:<>) :: WrapGHCiReadChannel -> a -> Tagged a
pattern w :<> a = (w, a)
infix 6 :<>


-- |  Represents a single line tagged with its source stream. Note that we use a regular 'String'
--    (instead of a t'NonEmpty' 'Char') as /GHCi/ output can produce empty lines arbitrarily.
type TaggedLine = Tagged String

-- |  A (possibly empty) list of 'TaggedLine's.
type TaggedLines = [TaggedLine]

-- |  A 'NonEmpty' list of 'TaggedLine's.
type TaggedLines1 = NonEmpty TaggedLine


--------------------------------------------------------------------------------
--  Writer channels and related types
--------------------------------------------------------------------------------

-- |  Types of writable channels into /GHCi/.
type data GHCiWriteChannel = GHCiInputChannel

-- |  Singleton for 'GHCiWriteChannel'.
data SingGHCiWriteChannel (channel :: GHCiWriteChannel) where
  SingGHCiInputChannel :: SingGHCiWriteChannel GHCiInputChannel

