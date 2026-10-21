{- |
    Module      : System.IO.Streams
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

module System.IO.Streams where


import Data.List.NonEmpty ( NonEmpty )


-- |  Abstract writeable stream with definable native type.
class WriterStream stream where
  -- |  The "native" type that is naturally written or read in the stream, to a specific channel.
  --    Defaults to a 'String'. Its data constructors need not be exported.
  type NativeWriteType stream
  type NativeWriteType stream = String
  -- |  The user-facing type that needs to be encoded before a write. Defaults to a 'String'.
  --    Its data constructors need not be exported.
  type UserWriteType stream
  type UserWriteType stream = String
  -- |  Function in charge of encoding the stream for writing. It's simply 'id'
  --    for the default 'WrittenType'.
  encode :: stream -> UserWriteType stream -> NativeWriteType stream
  default encode  :: (UserWriteType stream ~ String, NativeWriteType stream ~ String)
                  => stream
                  -> UserWriteType stream
                  -> NativeWriteType stream
  encode _ = id
  -- |  Writes to the stream and channel using its ["native"]('WrittenType') datatype.
  writeStreamRaw :: stream -> NativeWriteType stream -> IO ()
  -- |  Flush the stream. Non-buffered streams can make this a no-op.
  flushStream :: stream -> IO ()
  {-# MINIMAL writeStreamRaw, flushStream #-}


-- |  Writes normal data to a stream, automatically converting it into any internal format before writing if needed.
writeStream :: WriterStream stream => stream -> UserWriteType stream -> IO ()
writeStream stream = writeStreamRaw stream . encode stream


-- |  Abstract readable stream
class ReaderStream stream where
  -- |  The "native" type that is naturally written or read in the stream. Defaults to a 'String'.
  --    Its data constructors need not be exported.
  type NativeReadType stream
  type NativeReadType stream = NonEmpty Char
  -- |  The user-facing type that represents read content.
  type UserReadType stream
  type UserReadType stream = NonEmpty Char
  -- |  Function in charge of decoding the stream after reading. It's simply 'id'
  --    for the default 'ReadType'.
  decode :: stream -> NativeReadType stream -> UserReadType stream
  default decode  :: (NativeReadType stream ~ NonEmpty Char, UserReadType stream ~ NonEmpty Char)
                  => stream
                  -> NativeReadType stream
                  -> UserReadType stream
  decode _ = id
  -- |  Read from the stream in its native format
  readStreamRaw :: stream -> IO (NativeReadType stream)
  -- |  Indicates if the stream is ready to be read
  isStreamReady :: stream -> IO Bool
  {-# MINIMAL readStreamRaw, isStreamReady #-}


-- |  Reads normal data from a stream, automatically converting from any internal format before returning if needed.
readStream :: ReaderStream stream => stream -> IO (UserReadType stream)
readStream stream = decode stream <$> readStreamRaw stream

