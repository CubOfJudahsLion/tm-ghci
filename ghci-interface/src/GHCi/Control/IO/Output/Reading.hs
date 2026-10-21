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
  , checkGHCiReadableHandle
  , mkGHCiReader
  , mkGHCiReaderE
  ) where


import Common.Control.IO.Streams ( ReaderStream(..) )
import Control.Arrow ( Arrow((&&&)), (>>>) )
import Control.Monad.Except ( MonadError(throwError), ExceptT )
import Control.Monad.Trans ( MonadTrans(lift) )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty, toList )
import Data.Maybe ( fromMaybe )
import GHCi.Control.IO.Types
    ( GHCiReadChannel
    , SingGHCiReadChannel
    , TaggedLines1
    , WrapGHCiReadChannel(MkWrapGHCiReadChannel)
    , pattern (:<>)
    )
import System.IO ( Handle, hIsReadable, hReady )
import System.IO.Strict ( readAvailable )


--  Splits text into lines and tags them with the provided 'SingGHCiReadChannel'.
splitAndTag :: SingGHCiReadChannel channel  -- ^  Channel with which to tag the resulting lines
            -> NonEmpty Char                -- ^  Text to split
            -> TaggedLines1                 -- ^  Returns the list of tagged lines
splitAndTag sing  =   toList
                  >>> (:| []) &&& (lines >>> nonEmpty)
                  >>> uncurry fromMaybe
                  >>> fmap ((MkWrapGHCiReadChannel sing) :<>)


-- |  A handle that's been proven to be readable.
newtype GHCiReadableHandle = MkGHCiReadableHandle Handle

-- |  Creates a 'GHCiReadableHandle' from a normal 'Handle', provided the given handle is actually readable.
checkGHCiReadableHandle :: Handle -> ExceptT String IO GHCiReadableHandle
checkGHCiReadableHandle !h = do
  lift (hIsReadable h) >>= \case
    True  -> pure $ MkGHCiReadableHandle h
    _     -> throwError $  "GHCi.Control.IO.Output.Reading.checkGHCiReadableHandle: Error: <"
                        ++ show h
                        ++ "> isn't readable."


-- |  Stores a readable stream from a client process. This is an output stream
--    with a reversed direction: we're reading from @stdout@ or @stderr@.
data GHCiReader (channel :: GHCiReadChannel) = MkGHCiReader
  { singleton   :: !(SingGHCiReadChannel channel)  --  The tag we'll use in our output
  , waitMillis  :: !Int                         --  Grace period between reads
  , handle      :: !Handle                      --  The handle we read from.
  , other       :: !Handle                      --  The other handle we must be aware of, in case the client alternates streams.
  }

-- |  Smart constructor for a 'GHCiReader'. Requires [readable handles](GHCiReadableHandle).
mkGHCiReader  :: SingGHCiReadChannel channel -- ^  [Channel tag](GHCiReadChannel) to use in output
              -> Int                      -- ^  How long to allow between character reads. Zero means synchronous reading.
              -> GHCiReadableHandle       -- ^  'Handle' to read from.
              -> GHCiReadableHandle       -- ^  A second 'Handle' to watch. See 'readAvailable' for details.
              -> GHCiReader channel       -- ^  Returns a 'GHCiReader'.
mkGHCiReader singleton waitMillis (MkGHCiReadableHandle handle) (MkGHCiReadableHandle other) =
  MkGHCiReader { singleton, waitMillis, handle, other }


-- |  Monadic version of 'mkGHCiReader' within the @'ExceptT' 'String' 'IO'@ transformer.
mkGHCiReaderE :: SingGHCiReadChannel channel
              -> Int
              -> ExceptT String IO GHCiReadableHandle
              -> ExceptT String IO GHCiReadableHandle
              -> ExceptT String IO (GHCiReader channel)
mkGHCiReaderE singleton waitMillis tHandle tOther = do
  handle <- tHandle
  other  <- tOther
  pure $ mkGHCiReader singleton waitMillis handle other
{-# INLINABLE mkGHCiReaderE #-}


--  Instance for 'ReaderStream' for 'GHCiReader'. Any /GHCi/ reader (client) stream
--  reads 'NonEmpty Char's and produces a non-empty 'TaggedLines1'
instance ReaderStream (GHCiReader (channel :: GHCiReadChannel)) where
  type UserReadType (GHCiReader channel) = TaggedLines1
  decode (MkGHCiReader { singleton }) = splitAndTag singleton
  isStreamReady (MkGHCiReader { handle }) = hReady handle
  readStreamRaw (MkGHCiReader { waitMillis, handle, other }) = readAvailable waitMillis handle (Just other)

