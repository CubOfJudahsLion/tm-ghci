{-|
Module      : Interface
Description : A simple GHCi plugin for TeXmacs
Copyright   : (c) Alexander Feterman-Naranjo, 2023
License     : MIT
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX

This plugin allows running /GHCi/ sessions inside /TeXmacs/.
Currently, it provides only basic functionality, i.e., no project
options. Its only amenity is the use of the actual /GHCi/ prompt,
allowing customizations to be seen in /TeXmacs/.
-}

module Main where


import Data.Char (chr, isSpace)
import Control.Monad (void)
import GHC.Conc (par, pseq)
import System.IO
import System.Process (proc, CreateProcess(..), StdStream(CreatePipe), withCreateProcess)


-- |  The characters acting as brackets for every data block for TeXmacs.
dataBegin, dataEnd :: Char
dataBegin   = chr  2
dataEnd     = chr  5
--dataEscape  = chr 27


-- |  TeXmacs-wrap. Sets the proper data bracketing markers
-- |  around expressions.
tmWrap :: Bool -> String -> String
tmWrap withPrompt s =
  if withPrompt
    then
      --  Render all lines as verbatim, except for the
      --  last one which renders as a prompt
      let sLines    = lines s
          response  = unlines $ init sLines
          prompt    = last sLines
      in      dataBegin : "verbatim:" ++ response ++ dataEnd
          :   dataBegin : "prompt#"   ++ prompt   ++ dataEnd : ""
    else
      --  Render all as verbatim
      dataBegin : "verbatim:" ++ s ++ dataEnd : ""


-- |  An stream reader. Returns a wrapped string if there's
-- |  data available, Nothing otherwise.
readH :: Handle -> IO (Maybe String)
readH h = read' Nothing
  where
    --  Reads as much as possible from the given
    --  stream, which is assumed to be ready.
    getAll :: (String -> String) -> IO String
    getAll f = do
      ready <- hReady h
      if not ready
        then  pure $ f []
        else  do
                ch <- hGetChar h
                getAll $ f . (ch :)
    --  The worker function. Carries an accumulate
    --  and can wait between data bursts.
    read' :: Maybe String -> IO (Maybe String)
    read' sAcc = do
      ready <- hWaitForInput h 750
      if not ready
        then  pure sAcc
        else  do
                line <- getAll id
                read' $ sAcc <> Just line


-- |  A stream writer. Returns a wrapped
-- |  Boolean telling whether output was
-- |  possible or not.
writeH :: Handle -> String -> IO Bool
writeH h s = do
    writable <- hIsWritable h
    if writable
      then  do
              hPutStrLn h s
              pure True
      else  pure False


-- |  Reads one line from the stream, unless the user
-- |  provides a multi-line command starting with :{
readCommand :: IO String
readCommand = do
  firstLine <- getLine
  if trim firstLine == ":{"
    then  readRest ":{"
    else  pure firstLine
  where
    --  Trims the whites out of both ends
    trim :: String -> String
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    --  Reads several lines until it finds the closing ":}"
    readRest :: String -> IO String
    readRest acc = do
      nextLine <- getLine
      if trim nextLine == ":}"
        then  pure $ acc ++ "\n:}"
        else  readRest $ acc ++ '\n' : nextLine


-- |  Loop body. Runs after intro text.
loop :: Handle -> Handle -> Handle -> IO ()
loop hIn hOut hErr = do
  userInput <- readCommand
  written   <- writeH hIn userInput
  if not written then
    putStr $ tmWrap False "Pipe closed."
  else do
    --  Read stderr first, then concat.
    --  This ensures the prompt is the last line.
    mErr  <- readH hErr
    mOut  <- readH hOut
    let mResponse = mErr <> mOut
    case mResponse of
      Nothing       ->  putStr $ tmWrap False "Timed out."
      Just response ->  do
                          putStr $ tmWrap True response
                          loop hIn hOut hErr


-- |  Setup and main loop.
main :: IO ()
main = void $ withCreateProcess
  ((proc "ghci" ["-fdiagnostics-color=never"])
   { std_in  = CreatePipe
   , std_out = CreatePipe
   , std_err = CreatePipe}) -- need all three to communicate with ghci
  (\ mhIn mhOut mhErr _ -> case (mhIn, mhOut, mhErr) of
    (Just hIn, Just hOut, Just hErr)  -> do
      -- So flushing is not required:
      hSetBuffering hIn     NoBuffering
      hSetBuffering hOut    NoBuffering
      hSetBuffering stdin   NoBuffering
      hSetBuffering stdout  NoBuffering
      -- Give it plenty of time to initialize
      let imOut =   pseq hOut $ do
                      hasOutput <- hWaitForInput hOut 7500
                      if hasOutput
                        then  readH hOut
                        else  pure Nothing
      let imErr =   par hErr $ do
                      hasError  <- hWaitForInput hErr 7500
                      if hasError
                        then  readH hErr
                        else  pure Nothing
      mOut      <-  imOut
      mErr      <-  pseq imErr imErr
      mHeader   <-  imOut <> imErr
      case mHeader of
        Just header -> do
          putStr $ tmWrap True header
          loop hIn hOut hErr
        _           -> do
          putStr $ tmWrap False "Timed out."
    _                                 ->
      putStr $ tmWrap False "Pipe creation failed.")
