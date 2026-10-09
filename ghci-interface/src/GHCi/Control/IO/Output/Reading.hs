{- |
    Module      : GHi.Control.IO.Output.Reading
    Description : Low-level routines for reading /GHCi/ output
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Low-level routines for reading /GHCi/ output.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module GHCi.Control.IO.Output.Reading 
  ( GHCiReader
  , GHCiReadableHandle
  , mkClientReader
  ) where

import Common.Control.IO.Streams
import Control.Arrow ( Arrow((&&&)), (>>>) )
import Control.DeepSeq ( (<$!!>) )
import Control.Monad.Except
import Control.Monad.Trans
import Data.List.NonEmpty ( NonEmpty(..), toList, nonEmpty )
import Data.Maybe ( fromMaybe )
import GHCi.Control.IO.Types
import System.IO
import System.IO.Strict
import Data.Functor.Contravariant (Equivalence(getEquivalence))


--  Reads text using 'readAvailable', splitting it into lines and tagging them with the
--  provided 'OutputTag'.
splitAndTag :: SGHCiReadChannel channel -- ^  Tag to attach
            -> NonEmpty Char            -- ^  Text to split
            -> TaggedLines1 channel     -- ^  Returns the list of tagged lines
splitAndTag tag =   toList
                >>> (:| []) &&& (lines >>> nonEmpty)
                >>> uncurry fromMaybe
                >>> fmap (tag :@)


-- |  A handle that's been proven readable.
newtype GHCiReadableHandle = MkGHCiReadableHandle Handle

-- |  Creates a 'GHCiReadableHandle' from a normal 'Handle', provided the given handle is actually readable.
checkGHCiReadableHandle :: Handle -> ExceptT String IO GHCiReadableHandle
checkGHCiReadableHandle !h = do
  lift (hIsReadable h) >>= \case
    True  -> pure $ MkGHCiReadableHandle h
    _     -> throwError $   "GHCi.Control.IO.Output.Reading.checkGHCiReadableHandle: Error: <"
                        ++  show h
                        ++  "> isn't readable."


-- |  Stores a readable stream from a client process. This is an output stream
--    with a reversed direction: we're reading from @stdout@ or @stderr@.
data GHCiReader (channel :: GHCiReadChannel) = MkGHCiReader
  { tag         :: !(SGHCiReadChannel channel)  --  The tag we'll use in our output
  , waitMillis  :: !Int                         --  Grace period between reads
  , handle      :: !Handle                      --  The handle we read from.
  , other       :: !Handle                      --  The other handle we must be aware of, in case the client alternates streams.
  }

-- |  Smart constructor for a 'GHCiReader'. Requires [readable handles](GHCiReadableHandle).
mkClientReader  :: SGHCiReadChannel channel -- ^  [Channel tag](GHCiReadChannel) to use in output
                -> Int                      -- ^  How long to allow between character reads. Zero means synchronous reading.
                -> GHCiReadableHandle       -- ^  'Handle' to read from.
                -> GHCiReadableHandle       -- ^  A second 'Handle' to watch. See 'readAvailable' for details.
                -> GHCiReader channel       -- ^  Returns a 'GHCiReader' if the handles are readable, a string error otherwise.
mkClientReader tag waitMillis (MkGHCiReadableHandle handle) (MkGHCiReadableHandle other) =
  MkGHCiReader { tag, waitMillis, handle, other }


--  Make 'GHCiReader's work with 
instance CodedStream (GHCiReader (channel :: GHCiReadChannel)) where
  type UserType (GHCiReader channel) = TaggedLines1 channel
  type NativeType (GHCiReader channel) = NonEmpty Char

instance ReaderStream (GHCiReader (channel :: GHCiReadChannel)) where
  decode (MkGHCiReader { tag }) = splitAndTag tag
  isStreamReady (MkGHCiReader { handle }) = hReady handle
  readRawStream (MkGHCiReader { waitMillis, handle, other }) = readAvailable waitMillis handle (Just other)

