{- |
    Module      : GHCI Interface
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


import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Char (chr, isSpace)
import System.Exit (die)
import System.IO  ( Handle, stdin, stdout
                  , hReady, hWaitForInput, hIsClosed
                  , hGetChar, hPutStrLn
                  , BufferMode(NoBuffering), hSetBuffering
                  )
import System.Process (proc, CreateProcess(..), StdStream(CreatePipe), withCreateProcess)


-- |  The characters acting as brackets for every data block for TeXmacs.
dataBegin, dataEnd :: Char
dataBegin   = chr  2
dataEnd     = chr  5
-- dataEscape  = chr 27 -- ^ used to escape @dataBegin@ or @dataEnd@.


-- |  TeXmacs-wrap. Sets the proper data bracketing markers
-- |  around string data.
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


-- |  Waits for either h1 or h2 (or both) to be ready for reading.
-- |  The first handle has a higher priority, and is always returned
-- |  first if both are ready to be read.
waitReadable :: Handle -> Handle -> IO [Handle]
waitReadable h1 h2 = waitFirst 0
  where
    --  Waits for the first handle to be ready, or both
    --  if they have readable data.
    waitFirst :: Int -> IO [Handle]
    waitFirst waitedMillis =
      --  Will wait for 5 secs tops.
      if waitedMillis >= 5000
        --  if it's past that, return
        -- zero handles--the 'stop' signal
        then  pure []
        else  do
          h1Eof   <- hIsClosed h1
          h2Eof   <- hIsClosed h2
          if h1Eof || h2Eof
            then
              --  If any of the handles closed, return stop
              pure []
            else  do
              --  Not ended? Then check for readiness.
              h1Ready <- hReady h1
              h2Ready <- hReady h2
              case (h1Ready, h2Ready) of
                (True, True)    -> pure [h1, h2]
                (True, False)   -> timedWait ([h1] ++) h2
                (False, True)   -> timedWait (++ [h2]) h1
                (False, False)  -> threadDelay 25_000 >> waitFirst (waitedMillis + 25)
    --
    --  Once the first handle is acquired, the second one is
    --  given a limited wait time for readiness.
    timedWait :: ([Handle] -> [Handle]) -> Handle -> IO [Handle]
    timedWait accum handle = do
      handleReady <- hWaitForInput handle 750
      pure $ accum $ [handle | handleReady]


-- |  Main loop of GHCi interaction.
mainLoop :: Handle -> Handle -> Handle -> IO ()
mainLoop hIn hOut hErr = do
  --  First of all, no buffering. We need real-time data readiness.
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering hIn    NoBuffering
  hSetBuffering hOut   NoBuffering
  hSetBuffering hErr   NoBuffering
  --  Then we leave the logic to a worker function
  --  to avoid passing handles over and over.
  loop

  where
    loop :: IO ()
    loop = do
      handles       <-  waitReadable hErr hOut
      let nHandles  =   length handles
      if nHandles == 0
        then  pure () -- no handles = pipe(s) closed
        else  do
          putChar dataBegin
          forM_ (zip [1..] handles) (\ (n, h) -> do
            readData    <-  readAllOutput h
            let isLast  =   n == nHandles
                prnFn   =   if isLast then putStr else putStrLn
                wrapped =   tmWrap isLast readData
            prnFn wrapped)
          putChar dataEnd
          --userInput     <- getUserInput
          userInput     <- getLine
          hPutStrLn hIn userInput
          if trim userInput == ":q"
            then  do
              putStrLn "Leaving GHCi."
              pure ()
            else  do
              threadDelay 10_000
              loop

    --  This reads all of the available data of a stream
    readAllOutput :: Handle -> IO String
    readAllOutput h = read' id
      where
        read' :: (String -> String) -> IO String
        read' accumFn = do
          ready <- hReady h
          if not ready then
            pure $ accumFn ""
          else do
            ch <- hGetChar h
            read' (accumFn . (ch :))

    --  Trims a text from ending and starting spaces
    trim :: String -> String
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace


-- |  @main@ just spawns the process and invokes the loop.
main :: IO ()
main = do
  let procDesc  = (proc "ghci" ["-fdiagnostics-color=never"])
                    { std_in  = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }
  withCreateProcess procDesc
                    (\ mIn mOut mErr _ ->
                        case (mIn, mOut, mErr) of
                          (Just hIn, Just hOut, Just hErr)  ->
                            mainLoop hIn hOut hErr
                          _                                 ->
                            die "Could not create pipe(s).")

