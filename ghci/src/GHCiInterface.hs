{- |
    Module      : GHCiInterface
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

    The entirety of the plugin is contained in this file. The TeXmacs
    interface is quite simple and I haven't used packages outside the
    base Haskell distribution (through ghcup) to keep it that way.
-}


--{-# LANGUAGE LambdaCase #-}

module Main where


import Prelude hiding (putChar, getChar, putStr, putStrLn)
import Control.Concurrent (threadDelay)
--import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Char (chr, isSpace, ord)
import Data.List (intercalate)
import System.Exit (die)
import System.IO  ( Handle, stdin, stdout
                  , hReady, hWaitForInput, hIsClosed
                  , hGetChar, hGetLine
                  , hPutChar, hPutStr, hPutStrLn, hFlush
                  )
import System.Process ( proc, withCreateProcess, CreateProcess(..)
                      , StdStream(CreatePipe)
                      )
import Text.ParserCombinators.ReadP
--import Text.Printf (printf)


-- |  @dataBegin@ and @dataEnd@ act as brackets for every data block for TeXmacs.
-- |  @dataEscape@ preceding them makes them act as normal characters.
dataBegin, dataEnd, dataEscape :: Char
dataBegin   = chr  2
dataEnd     = chr  5
dataEscape  = chr 27


-- |  Sets a data type for stream identification
data StreamType = DataStream    -- ^  Good data stream, e.g., stdout
                | ErrorStream   -- ^  Error stream, such as stderr

--  Parses any set of two or more spaces
parseMultiSpaces :: ReadP String
parseMultiSpaces = do
    s1 <- satisfy isSpace
    s2 <- satisfy isSpace
    ss <- munch   isSpace
    pure $ "\\verb|" ++ s1 : s2 : ss ++ "|"
--
--  Parses most of LaTeX's "special meaning" characters
parseLaTeXSpecials :: ReadP String
parseLaTeXSpecials  =   ("\\verb|" ++) . (++ "|")
                    <$> munch1 (`elem` "^%${}[]_#&~\\")
-- --
-- --  Parses a circumflex accent symbol
-- parseCircum :: ReadP String
-- parseCircum = "\\textasciicircum{}" <$ char '^'
-- --
-- --  Parses a less-than symbol
-- parseLess :: ReadP String
-- parseLess = "\\textless{}" <$ char '<'
-- --
-- --  Parses a greater-than symbol
-- parseGreater :: ReadP String
-- parseGreater = "\\textgreater{}" <$ char '>'
--
--  Parses TeXmacs's data-bracketing characters, including
--  the escaping character itself
parseTeXmacsDataMarkers :: ReadP String
parseTeXmacsDataMarkers =   (dataEscape :) . (: "")
                        <$> satisfy (`elem` [ dataBegin
                                            , dataEnd
                                            , dataEscape])
--
--  Consumes any other character, returning it
parseOther :: ReadP String
parseOther = (: "") <$> get
--
--  Parses one of any of the above
parseAny :: ReadP String
parseAny  =   parseMultiSpaces
          <++ parseLaTeXSpecials
          -- <++ parseCircum
          -- <++ parseLess
          -- <++ parseGreater
          <++ parseTeXmacsDataMarkers
          <++ parseOther
          <++ pure ""
--
--  The above parser, compiled into a
--  function that returns possible partial
--  parses and remaining substrings
parseAnyS :: ReadS String
parseAnyS = readP_to_S parseAny
--
--  Parse an element at a time to minimize
--  the size of intermediate parse lists
rewriteForLatex :: String -> String
rewriteForLatex = runParser ""
  where
    runParser :: String -> String -> String
    runParser acc remaining =
      let (partialParse, remaining')  = last $ parseAnyS remaining
          acc'                        = acc ++ partialParse
      in  if null remaining'
            then  acc'
            else  runParser acc' remaining'


--  Fixes a series of lines into a single LaTeX-encoded string
fixLines :: Bool -> [String] -> String
fixLines isErr  = let redFn s   = if null s
                                    then s
                                    else "\\red " ++ s
                      tintAdder = if isErr
                                    then  (redFn .)
                                    else  id
                      rewriter  = tintAdder rewriteForLatex
                  in  intercalate "\\\\\n"
                    . fmap rewriter
  --where
    ----  Fixes special characters so they appear properly
    ----  inside Scheme strings
    --fixChars :: String -> String
    --fixChars = foldr ($) ""
    --         . fmap (\case
    --                  --  Special TeXmacs stream characters need
    --                  --  to be preceded by an escape
    --                  ch  |  ch == dataBegin
    --                      || ch == dataEnd
    --                      || ch == dataEscape -> (dataEscape :) . (ch :)
    --                  --  Characters with special meaning
    --                  --  in LaTeX often need to be escaped
    --                  --  or shown as tags
    --                  ' '                     -> ("\\verb| |" ++)
    --                  '%'                     -> ("\\verb|%|" ++)
    --                  '$'                     -> ("\\verb|$|" ++)
    --                  '{'                     -> ("\\verb|{|" ++)
    --                  '}'                     -> ("\\verb|}|" ++)
    --                  '['                     -> ("\\verb|[|" ++)
    --                  ']'                     -> ("\\verb|]|" ++)
    --                  '_'                     -> ("\\verb|_|" ++)
    --                  '#'                     -> ("\\verb|#|" ++)
    --                  '&'                     -> ("\\verb|&|" ++)
    --                  '^'                     -> ("\\textasciicircum{}" ++)
    --                  '\\'                    -> ("\\verb|\\|" ++)
    --                  '~'                     -> ("\\verb|~|" ++)
    --                  '<'                     -> ("\\textless{}" ++)
    --                  '>'                     -> ("\\textgreater{}" ++)
    --                  ch                      -> (ch :))
-- |  TeXmacs-wrap. Sets the proper data bracketing markers
-- |  around string data.
tmWrap :: StreamType -> String -> String

tmWrap ErrorStream s =
  --  No need to process an error response
  let errStr = fixLines True $ lines s
  in  dataBegin : "latex:" ++ errStr ++ dataEnd : ""

tmWrap DataStream  s =
  --  Render all lines as LaTeX, except for the
  --  last one which renders as a prompt.
  --
  --  NOTE:
  --  I tried using the "scheme" encoding, but
  --  current TeXmacs uses libguile 1.8 and it
  --  doesn't support Unicode characters.
  let sLines              = lines s
      (response, prompt)  = if null sLines
                              then  ("", "")
                              else  ( fixLines False $ init sLines
                                    , last sLines)
  in  if null response
        then
          dataBegin : "prompt#" ++ prompt   ++ dataEnd : ""
        else
          dataBegin : "latex:"  ++ response ++ dataEnd :
          dataBegin : "prompt#" ++ prompt   ++ dataEnd : ""


-- |  Alias for a handle tagged with its type
type TaggedStream = (StreamType, Handle)


-- |  Waits for either hErr or hOut (or both) to be ready for reading.
-- |  The error handle has a higher priority, and is always returned
-- |  first if both are ready to be read.
waitReadable  ::  Handle              -- ^  Error stream
              ->  Handle              -- ^  Data Stream
              ->  IO [TaggedStream]   -- ^  An array of tagged ready handles
waitReadable hErr hOut = waitFirst 0
  where
    --  Waits for the first handle to be ready, or both
    --  if they have readable data.
    waitFirst :: Int -> IO [TaggedStream]
    waitFirst waitedMillis =
      --  Will wait for 5 secs tops.
      if waitedMillis >= 5000
        --  if it's past that, return
        -- zero handles--the 'stop' signal
        then  pure []
        else  do
          hErrEof   <- hIsClosed hErr
          hOutEof   <- hIsClosed hOut
          if hErrEof || hOutEof
            then
              --  If any of the handles closed, return stop
              pure []
            else  do
              --  Not ended? Then check for readiness.
              hErrReady <- hReady hErr
              hOutReady <- hReady hOut
              case (hErrReady, hOutReady) of
                (True,  True)   -> pure      [    (ErrorStream, hErr),     (DataStream, hOut)]
                (True,  False)  -> timedWait (   [(ErrorStream, hErr)] ++)  DataStream  hOut
                (False, True)   -> timedWait (++ [(DataStream,  hOut)])     ErrorStream hErr
                (False, False)  -> threadDelay 25_000 >> waitFirst (waitedMillis + 25)
    --
    --  Once the first handle is acquired, the second one is
    --  given a limited wait time for readiness.
    timedWait :: ([TaggedStream] -> [TaggedStream]) -> StreamType -> Handle -> IO [TaggedStream]
    timedWait accum streamType handle = do
      handleReady <- hWaitForInput handle 750
      pure $ accum [(streamType, handle) | handleReady]


-- |  Main loop of GHCi interaction.
mainLoop :: Handle -> Handle -> Handle -> IO ()
mainLoop hIn hOut hErr = loop
  where
    loop :: IO ()
    loop = do
      taggedHandles <-  waitReadable hErr hOut
      let nHandles  =   length taggedHandles
      if nHandles == 0
        then  pure () -- no handles = pipe(s) closed
        else  do
          hPutChar stdout dataBegin
          hPutStr stdout "verbatim:"
          forM_ (zip [1..] taggedHandles) (\ (index, (sType, handle)) -> do
            readData    <-  readAllOutput handle
            let isLast  =   index == nHandles
                hPrnFn  =   if isLast then hPutStr else hPutStrLn
                wrapped =   tmWrap sType readData
            hPrnFn stdout wrapped)
          hPutChar stdout dataEnd
          hFlush stdout
          rawInput      <-  getLine
          let trimInput =   trim rawInput
              input     =   if      length trimInput <= length quitCmd
                                &&  all (uncurry (==)) (zip quitCmd trimInput)
                              then  ""
                              else  rawInput
          hPutStrLn hIn input
          hFlush hIn
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

    --  The command to quit GHCi. We need to exclude it from
    --  being sent to GHCi as TeXmacs much preferes to terminate
    --  the process itsef.
    quitCmd :: String
    quitCmd = ":quit"


-- |  @main@ just spawns the process and invokes the loop.
main :: IO ()
main = do
  let procDesc  = (proc "ghci" ["-fdiagnostics-color=never", "-fprint-unicode-syntax"])
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

