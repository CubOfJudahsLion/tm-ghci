{- |
    Module      : GHi.Control.IO.Input.Writing
    Description : Low-level routines for writing into /GHCi/
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    Low-level routines for writing into /GHCi/'s input.
-}


{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module GHCi.Control.IO.Input.Writing
  ( GHCiWritableHandle
  , GHCiWriter
  , checkGHCiWritableHandle
  , mkGHCiWriter
  , mkGHCiWriterE
  ) where


import Common.Control.IO.Streams ( WriterStream(..) )
import Control.Monad.Except ( MonadError(throwError), ExceptT )
import Control.Monad.Trans ( MonadTrans(lift) )
import GHCi.Control.IO.Types
import System.IO ( hFlush, hIsWritable, hPutStr, Handle )


-- |  A handle that's been proven to be writable.
newtype GHCiWritableHandle = MkGHCiWritableHandle Handle

-- |  Creates a 'GHCiWritableHandle' from a normal 'Handle', provided the given handle is actually readable.
checkGHCiWritableHandle :: Handle -> ExceptT String IO GHCiWritableHandle
checkGHCiWritableHandle !h = do
  lift (hIsWritable h) >>= \case
    True  -> pure $ MkGHCiWritableHandle h
    _     -> throwError $   "GHCi.Control.IO.Input.Writing.checkGHCiWritableHandle: Error: <"
                        ++  show h
                        ++  "> isn't writable."


-- |  Stores a writable stream for a client process. This is an input stream
--    with a reversed direction: we're writing into @stdin@.
newtype GHCiWriter (channel :: GHCiWriteChannel) = MkGHCiWriter Handle

-- |  Smart constructor for a 'GHCiWriter'. Requires [readable handles](GHCiWritableHandle).
mkGHCiWriter  :: SingGHCiWriteChannel channel -- ^  [Channel](GHCiWriteChannel) for type resolution
              -> GHCiWritableHandle           -- ^  'Handle' to write into
              -> GHCiWriter channel           -- ^  Returns a 'GHCiWriter'
mkGHCiWriter _ (MkGHCiWritableHandle handle) = MkGHCiWriter handle


-- |  Monadic version of 'mkGHCiWriter' within the @'ExceptT' 'String' 'IO'@ transformer.
mkGHCiWriterE :: SingGHCiWriteChannel channel
              -> ExceptT String IO GHCiWritableHandle
              -> ExceptT String IO (GHCiWriter channel)
mkGHCiWriterE chan tHandle = do
  handle <- tHandle
  pure $ mkGHCiWriter chan handle
{-# INLINABLE mkGHCiWriterE #-}


--  Instance for 'WriterStream' for 'GHCiWriter'. Any /GHCi/ reader (client) stream
--  reads 'NonEmpty Char's and produces a non-empty 'TaggedLines1'
instance WriterStream (GHCiWriter (channel :: GHCiWriteChannel)) where
  writeStreamRaw (MkGHCiWriter h) = hPutStr h
  flushStream (MkGHCiWriter h) = hFlush h

