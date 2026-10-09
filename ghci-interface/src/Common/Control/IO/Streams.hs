{- |
    Module      : Common.Control.IO.Streams
    Description : IO Streams
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    This modules define abstract stream interfaces with encoding or decoding capabilities,
    and which expose their own internal user-facing types. Decoding/encoding are optional
    as the internal types can be simple 'String' or 'NonEmpty Char'.
-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Control.IO.Streams where


import Data.List.NonEmpty ( NonEmpty, toList )
import Control.Applicative (optional)
import System.Process (CreateProcess(use_process_jobs))
import GHC.Generics (Selector(selSourceUnpackedness))


class CodedStream stream where
  -- |  The "native" type that is naturally written or read in the stream. Defaults to a 'String'.
  --    Its data constructors need not be exported.
  type NativeType stream
  -- |  The "user-facing" type that results from decoding after a read, or that the user sends.
  --    for writing (and is then internally encoded.) Defaults to a 'String'. Its data constructors
  --    need not be exported.
  type UserType stream


-- |  Abstract writable stream with optional encoding capabilities.
class CodedStream stream => WriterStream stream where
  -- |  Function in charge of encoding the stream for wriing. It's simply 'id'
  --    for the default 'WrittenType'.
  encode :: stream -> UserType stream -> NativeType stream
  default encode :: (UserType stream ~ String, NativeType stream ~ String) => stream -> UserType stream -> NativeType stream
  encode _ = id
  -- |  Writes to the stream using its ["native"]('WrittenType') datatype.
  writeRawStream :: stream -> NativeType stream -> IO ()
  -- |  Flush the stream. Non-buffered streams can make this a no-op.
  flushStream :: stream -> IO ()
  {-# MINIMAL writeRawStream, flushStream #-}


-- |  Writes normal data to a stream, automatically converting it into any internal format before writing if needed.
writeStream :: WriterStream stream => stream -> UserType stream -> IO ()
writeStream stream = writeRawStream stream . encode stream


-- |  Abstract readable stream with optional decoding capabilities.
class CodedStream stream => ReaderStream stream where
  -- |  Function in charge of decoding the stream after reading. It's simply 'id'
  --    for the default 'ReadType'.
  decode :: stream -> NativeType stream -> UserType stream
  default decode :: (NativeType stream ~ NonEmpty Char, UserType stream ~ NonEmpty Char) => stream -> NativeType stream -> UserType stream
  decode _ = id
  -- |  Read from the stream, producing its ["native"]('ReadType') datatype.
  readRawStream :: stream -> IO (NativeType stream)
  -- |  Indicates if the stream is ready to be read
  isStreamReady :: stream -> IO Bool
  {-# MINIMAL readRawStream, isStreamReady #-}


-- |  Reads normal data from a stream, automatically converting from any internal format after reading if needed.
readStream :: ReaderStream stream => stream -> IO (UserType stream)
readStream stream = decode stream <$> readRawStream stream

