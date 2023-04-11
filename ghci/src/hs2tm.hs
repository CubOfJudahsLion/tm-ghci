{- |
    Module      : hs2tm
    Description : A converter from Haskell (.hs, .hsc) to TeXmacs
    Copyright   : (c) Alexander Feterman-Naranjo, 2023
    License     : MIT
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    This converter has been embedded in the tm-ghci plugin for TeXmacs,
    but the program can be run from the command line.

    Do note that compilation of this file requires a couple of tricks,
    and that the current version of GHC must be the same as the one
    used to compile this executable.

    References in the comments to the Haskell 2010 Language Report
    are indicated as /"HLR"/ from here on out.
-}


{-# LANGUAGE LambdaCase #-}

module Main where


import Control.Arrow ((>>>))
import Control.Exception (IOException, catch)
import Data.Char (toLower, isSpace)
import Data.Function ((&))
import GitVersion (gitVersion)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.IO ( Handle, IOMode(ReadMode, WriteMode)
                 , stdin, stdout, stderr
                 , openFile, hGetContents, hPutStr, hClose
                 , utf8, hSetEncoding
                 )


-- |  Prints a small help text on purpose and usage.
showHelp :: IO ()
showHelp = do
  exeName       <-  getProgName
  let exeSpaces =   map (const ' ') exeName
  putStrLn $ exeName ++ ' ' : gitVersion ++ '\n' :
             "Transforms a Haskell source file into a TeXmacs file.\n\
             \Usage:\n\
             \  " ++ exeName   ++ " -[-]h[e[l[p]]]           : print this message\n\
             \  " ++ exeSpaces ++ "                            (overrides all other options)\n\
             \  " ++ exeName   ++ " [inputFile [outputFile]] : convert from inputFile to outputFile.\n\
             \  " ++ exeSpaces ++ "                            (default to stdin and stdout respectively)"


-- |  Checks whether help was requested through the command line,
-- |  which is passed a an array of strings
helpRequestedIn :: [String] -> Bool
helpRequestedIn = any (\ arg -> let arg' = dropWhile (== '-') $ map toLower arg
                                in  all (uncurry (==)) (zip arg' "help"))
                . filter (\case
                           ('-':_) -> True
                           _       -> False)


-- |  For characters that my or may not be expanded into strings.
data StrOrChr = Str String
              | Chr Char

-- |  Convert a series of @StrOrChr@ into a @String@
socToStr :: [StrOrChr] -> String
socToStr = foldr (\ soc acc ->  case soc of
                                  Str str ->  str ++ acc
                                  Chr chr ->  chr :  acc)
                 ""


-- |  Specifically processes Haskell source as a string into a form
-- |  that can be inserted into the document.
processHaskell :: String -> String
processHaskell s  = s
                  & fmap  (\case
                            '\f' -> '\n'     -- HLR: formfeed is considered a line separator
                            ch   -> ch)
                  & lines                    -- lines will split correctly now
                  & fmap  (   (\ s -> if all isSpace s
                                        then  "\\;"
                                        else  escapeChars s)
                          >>> ("  " ++)
                          >>> (++ "\n\n"))   -- chars are now correctly escaped,
                                             -- lines double-spaced and
                                             -- space-indented
                  & unlines
  where
    escapeChars :: String -> String
    escapeChars =   zip [0..]
                >>> fmap (\ (idx, ch) ->  case ch of
                                            '\t' -> Str $ replicate (8 - idx `div` 8) ' '
                                                      --  HLR: tab stops are 8 chars apart
                                            '\\' -> Str "\\\\"
                                            '|'  -> Str "\\|"
                                            ' '  -> Str "\\ "
                                            '<'  -> Str "<less>"
                                            '>'  -> Str "<gtr>"
                                            _    -> Chr ch)
                >>> socToStr


-- |  Processes the contents of the Haskell file, producing a suitable
-- |  text for the target file
convertContent :: String -> String
convertContent s = prologue ++ processHaskell s ++ epilogue
  where
    prologue :: String
    prologue = "<TeXmacs|2.1>\n\n\
               \<style|generic>\n\n\
               \<\\body>\n\n"
    --
    epilogue :: String
    epilogue = "</body>\n\n\
               \<\\initial>\n\
               \  <\\collection>\n\
               \    <associate|font-family|tt>\n\
               \    <associate|font-base-size|9>\n\
               \    <associate|font-size|1>\n\
               \    <associate|par-sep|0.1fn>\n\
               \    <associate|par-par-sep|0.1fn*>\n\
               \    <associate|preamble|false>\n\
               \  </collection>\n\
               \</initial>"


-- |  Handles conversion at the file level
fileConvert ::  Handle  -- ^  input stream
            ->  Handle  -- ^  output stream
            ->  IO ()
fileConvert hIn hOut = do
  content <- hGetContents hIn
  hSetEncoding hOut utf8
  hPutStr hOut $ convertContent content
  hClose hOut


-- |  Handles switches and stream selection
main :: IO ()
main = do
  args <- getArgs
  if helpRequestedIn args
    then  showHelp
    else  do
            exeName     <-  getProgName
            [hIn, hOut] <-  (case args of
                              []      ->  pure [stdin, stdout]
                              [i]     ->  sequence [openFile i ReadMode, pure stdout]
                              [i, o]  ->  sequence [openFile i ReadMode, openFile o WriteMode]
                              _       ->  die $  "Wrong number of arguments. Call "
                                              ++ exeName
                                              ++ " with --help for usage.")
                            `catch`
                              (\ (_ :: IOException) -> die "Couldn't open file.")
            fileConvert hIn hOut

