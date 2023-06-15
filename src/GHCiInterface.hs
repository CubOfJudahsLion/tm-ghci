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
    allowing customizations to be seen in /TeXmacs/, and completion.
-}


{-# LANGUAGE LambdaCase #-}
module Main where


import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void)
import Data.Char (chr, isSpace, ord)
import Data.List (intercalate, foldl')
import System.Exit (die)
import System.IO  ( Handle, {-stdin,-} stdout
                  , hReady, hWaitForInput, hIsClosed
                  , hGetChar
                  , {-hPutChar,-} hPutStr, hPutStrLn, hFlush
                  )
import System.Process ( proc, withCreateProcess, CreateProcess(..)
                      , StdStream(CreatePipe)
                      )
import Text.ParserCombinators.ReadP



{- ============ TeXmacs datablock prefixes and suffixes ============ -}

-- |  @dataBegin@ and @dataEnd@ act as brackets for every data block for TeXmacs.
-- |  @dataEscape@ preceding them makes them act as normal characters.
-- |  @dataCommand@ is used to receive commands from TeXmacs
dataBegin, dataEnd, dataEscape, dataCommand :: Char
dataBegin   = chr  2
dataEnd     = chr  5
dataEscape  = chr 27
dataCommand = chr 16



{- ============= Preprocessing strings for LaTeX output  ============ -}

-- |  Parses any set of two or more spaces
parseMultiSpaces :: ReadP String
parseMultiSpaces = do
    s1 <- satisfy isSpace
    s2 <- satisfy isSpace
    ss <- munch   isSpace
    pure $ "\\verb|" ++ s1 : s2 : ss ++ "|"


-- |  Parses most of LaTeX's "special meaning" characters
parseLaTeXSpecials :: ReadP String
parseLaTeXSpecials  = ("\\verb|" ++) . (++ "|") <$> munch1 (`elem` "^%${}[]_#&~\\")


-- |  Escape block delimiters if found in the stream itself
parseTeXmacsDataMarkers :: ReadP String
parseTeXmacsDataMarkers =   (dataEscape :) . (: "")
                        <$> satisfy (`elem` [ dataBegin
                                            , dataEnd
                                            , dataEscape])


-- |  Consumes any other character, returning it
parseOther :: ReadP Char
parseOther = get
{-# INLINE parseOther #-}


-- |  A class of parser that can become String Builders. We define
-- |  a string builder as a function @ReadP (String -> String)@.
-- |  Once converted to a ReadS and applied to a string, it yields
-- |  a function that /prepends/ its parser result to a @String@.
class StringBuilderParser p where
  makeStringBuilder :: ReadP p -> ReadP (String -> String)

-- |  String parsers as String Builders
instance StringBuilderParser String where
  makeStringBuilder :: ReadP String -> ReadP (String -> String)
  makeStringBuilder = ((++) <$>)

-- |  Char parsers as String Builders
instance StringBuilderParser Char where
  makeStringBuilder :: ReadP Char -> ReadP (String -> String)
  makeStringBuilder = ((:) <$>)


-- |  Parses one of any of the above, plus blank lines,
-- |  making them all string builders
parseAny :: ReadP (String -> String)
parseAny  =   makeStringBuilder parseMultiSpaces
          <++ makeStringBuilder parseLaTeXSpecials
          <++ makeStringBuilder parseTeXmacsDataMarkers
          <++ makeStringBuilder parseOther
          <++ pure id   -- We get here if it couldn't parse anything
                        -- (i.e., blank line); we just keep it as is

-- |  The above parser, compiled into a
-- |  function that returns possible partial
-- |  parses and remaining substrings
parseAnyS :: ReadS (String -> String)
parseAnyS = readP_to_S parseAny

-- |  Parse an element at a time to minimize
-- |  the size of intermediate parse lists
rewriteForLatex :: String -> String
rewriteForLatex = runParser id
  where
    runParser :: (String -> String) -> String -> String
    runParser acc remaining =
      let (partialParse, remaining')  = last $ parseAnyS remaining
          acc'                        = acc . partialParse
      in  if null remaining'
            then  acc' ""
            else  runParser acc' remaining'


-- |  Fixes a series of lines into a single LaTeX-encoded string
fixLines :: Bool -> [String] -> String
fixLines isErr  = let tintRed s = if null s
                                    then s
                                    else "\\red " ++ s
                      tintAdder = if isErr
                                    then  (tintRed .)
                                    else  id
                      rewriter  = tintAdder rewriteForLatex
                  in  intercalate "\\\\" . fmap rewriter


-- |  Sets a data type for stream identification
data StreamType = DataStream    -- ^  Good data stream, e.g., stdout
                | ErrorStream   -- ^  Error stream, such as stderr


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
  --  I tried using the "scheme:" format, but
  --  current TeXmacs uses libguile 1.8, whose
  --  Unicode support is basically non-existent.
  --  If you're interfacing with a REPL that
  --  uses Unicode, Scheme won't suit you.
  let sLines              = lines s
      (response, prompt)  = if null sLines
                              then  ("", "GHCi] ")
                              else  ( fixLines False $ init sLines
                                    , last sLines)
  in  if null response
        then
          dataBegin : "prompt#" ++ prompt   ++ dataEnd : ""
        else
          dataBegin : "latex:"  ++ response ++ dataEnd :
          dataBegin : "prompt#" ++ prompt   ++ dataEnd : ""



{- ============ I/O Handling ============ -}

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


-- |  This reads all of the available data of a stream, getting
-- |  a first character and then waiting 'till there's no more
readAll :: Handle -> IO String
readAll h = read' id
  where
    read' :: (String -> String) -> IO String
    read' accFn = do
      ch          <-  hGetChar h
      let accFn'  =   accFn . (ch :)
      ready       <-  hReady h
      if not ready
        then  pure $ accFn' ""
        else  read' accFn'


-- |  Much like @readAll@, this reads all of the available data o
-- |  a stream. Precondition: the stream is /ready/.
readAllIfReady :: Handle -> IO String
readAllIfReady h = read' id
  where
    read' :: (String -> String) -> IO String
    read' accumFn = do
      ready <- hReady h
      if not ready then
        pure $ accumFn ""
      else do
        ch <- hGetChar h
        read' (accumFn . (ch :))


-- |  Reads, processes and dispatches user input
handleInput :: Handle -> String -> IO ()
handleInput hIn rawInput = do
  let input = if isQuitCmd rawInput
                then  ""
                else  rawInput
  hPutStrLn hIn input
  hFlush hIn
  where
    --  This is a recognizer for a GHCi quit string:
    --  optional spaces, one or two colons, and one
    --  or more characters of the word quit, without
    --  extra non-spaces. The rest of the string is
    --  ignored.
    parseQuit :: ReadP ()
    parseQuit =             skipSpaces
              >>            char ':'
              >>  optional (char ':')
              >>            char 'q'
              >>  optional (char 'u'
              >>  optional (char 'i'
              >>  optional (char 't')))
              >>  (eof <++ void (satisfy isSpace))
    --
    --  The above parser compiled into a recognizer
    --  function that reads the string and returns
    --  booleans as parsing results.
    parseQuitS :: ReadS Bool
    parseQuitS = readP_to_S ((True <$ parseQuit) <++ pure False)
    --
    --  Recognizes whether a string is a quit command
    --  using the above parsers
    isQuitCmd :: String -> Bool
    isQuitCmd = fst . last . parseQuitS


--  Accepts a "simple" character (i.e., one that needs no '\'
--  prior) inside a string.
parseSimpleStringChar :: ReadP Char
parseSimpleStringChar = satisfy (\ ch -> ch /= '"' && ch /= '\\')

ord0 :: Int
ord0 = ord '0'

decimalDigit :: ReadP Int
decimalDigit  = (\ ch -> ord ch - ord0) <$> satisfy (`elem` "0123456789")

ordA :: Int
ordA = ord 'A'

orda :: Int
orda = ord 'a'

hexDigit :: ReadP Int
hexDigit  =     decimalDigit
          <++   ((\ ch -> ord ch - ordA + 10) <$> satisfy (`elem` "ABCDEF"))
          <++   ((\ ch -> ord ch - orda + 10) <$> satisfy (`elem` "abcdef"))

--  Parses a character requiring '\' prior.
parseCompositeStringChar :: ReadP Char
parseCompositeStringChar =  char '\\'
               >>          (char 'x' >> do h1 <- hexDigit
                                           h2 <- hexDigit
                                           pure $ chr $ 16 * h1 + h2)
               <++          char '"'
               <++          char '\\'
               <++          char '|'
               <++          char '('
               <++ ('\0' <$ char '0')
               <++ ('\a' <$ char 'a')
               <++ ('\b' <$ char 'b')
               <++ ('\f' <$ char 'f')
               <++ ('\n' <$ char 'n')
               <++ ('\r' <$ char 'r')
               <++ ('\t' <$ char 't')
               <++ ('\v' <$ char 'v')

parseStringChar :: ReadP Char
parseStringChar =   parseSimpleStringChar
                <++ parseCompositeStringChar

parseString :: ReadP String
parseString = do
  void $ char '"'
  str <- many parseStringChar
  void $ char '"'
  pure str

--  Parses the entire completion request and
--  produces the string and position
parseCompletionRequest :: ReadP (String, Int)
parseCompletionRequest = do
  skipSpaces
  void $ char dataCommand
  skipSpaces
  void $ char '('
  skipSpaces
  void $ string "complete"
  void $ munch1 isSpace
  strToComplete   <- parseString
  void $ munch1 isSpace
  cursorPosition  <-  foldl' (\ acc curr -> acc * 10 + curr) 0
                  <$> many1 decimalDigit
  skipSpaces
  void $ char ')'
  pure (strToComplete, cursorPosition)

--  Takes a string that may be a completion request, and
--  produces the string and position if that's the case.
getRequestParams :: String -> Maybe (String, Int)
getRequestParams s =
  let completions = readP_to_S parseCompletionRequest s
  in  if null completions
        then  Nothing
        else  Just $ fst $ last completions

-- |  Handles a completion request from TeXmacs
-- |  (activated by pressing tab while entering the next input)
handleCompletion :: Handle -> Handle -> String -> Int -> IO ()
handleCompletion hIn hOut request cursorPos = do
  putStrLn $ "Handling completion of string \"" ++ request ++ "\" at pos " ++ show cursorPos
  let root            =   take cursorPos request
      rootLength      =   length root
      rest            =   drop cursorPos request
      mapQuotes       =   foldr ($) ""
                      .   fmap (\case
                                 '"' -> ("\\\"" ++)
                                 ch  -> (ch :))
      responseHeader  =   dataBegin
                      :   "scheme:(tuple \""
                      ++  mapQuotes root
                      ++  "\" "
      responseTail    =   ')'
                      :   [dataEnd]
      mkResponse s    =   responseHeader ++ s ++ responseTail
  hPutStrLn hIn $ ":complete repl \"" ++ mapQuotes root ++ "\""
  hFlush hIn
  threadDelay 50_000
  rawCompletions      <-  lines <$> readAll hOut
  let response        =   mkResponse
                      $   if length rawCompletions <= 1
                            then  "\"\""
                            else  let completions =   filter (not . null)
                                                  $   drop (rootLength + 1) . init
                                                  <$> tail rawCompletions
                                  in    unwords
                                    $   ('"' :) . (++ "\"") . mapQuotes
                                    <$> completions
  putStrLn response
  hFlush stdout


-- |  A simple datatype for signaling continuation
data StopOrContinue  = Stop | Continue deriving (Eq)

-- |  Processes and relays all GHCi output
handleOutputStreams :: Handle -> Handle -> IO StopOrContinue
handleOutputStreams hErr hOut = do
  taggedHandles <-  waitReadable hErr hOut
  let nHandles  =   length taggedHandles
  if nHandles == 0
    then  do
      putStrLn "Pipes closed"
      pure Stop
    else  do
      putChar dataBegin
      putStr "verbatim:"
      forM_ (zip [1..] taggedHandles) (printOutput nHandles)
      putChar dataEnd
      hFlush stdout
      pure Continue
  where
    printOutput :: Int -> (Int, (StreamType, Handle)) -> IO ()
    printOutput nHandles (index, (sType, handle)) = do
      readData    <-  readAllIfReady handle
      let isLast  =   index == nHandles
          hPrnFn  =   if isLast
                        then  hPutStr
                        else  hPutStrLn
          wrapped =   tmWrap sType readData
      hPrnFn stdout wrapped


-- |  Main loop of GHCi interaction.
mainLoop :: Handle -> Handle -> Handle -> IO ()
mainLoop hIn hOut hErr = checkStreams
  where
    -- Checks the output of GHCi for data
    checkStreams :: IO ()
    checkStreams = do
      stopOrContinue <- handleOutputStreams hErr hOut
      case stopOrContinue of
        Stop      -> pure ()
        Continue  -> loop
    --
    --  Worker loop function that doesn't pass
    --  the handles over and over
    loop :: IO ()
    loop = do
      userInput     <-  getLine
      let reqParms  =   getRequestParams userInput
      case reqParms of
        Just (stringToComplete, cursorPosition) -> do
          handleCompletion  hIn hOut
                            stringToComplete
                            cursorPosition
          loop
        Nothing                                 -> do
          handleInput hIn userInput
          threadDelay 5000
          checkStreams

-- |  @main@ just spawns the process and invokes the loop.
main :: IO ()
main = do
  let procDesc  = (proc "ghci" [ "-fdiagnostics-color=never"
                               , "-fprint-unicode-syntax"])
                    { std_in  = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe }
  withCreateProcess procDesc
                    (\ mIn mOut mErr _ ->
                        case (mIn, mOut, mErr) of
                          (Just hIn, Just hOut, Just hErr)  ->
                            mainLoop hIn hOut hErr
                          _                                 ->
                            die "Could not create pipe(s).")

