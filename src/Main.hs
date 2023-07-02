{-|
Module      : Main
Description : Entry point to the GHCi/TeXmacs interface
Copyright   : (c) 2022,2023 Alexander Feterman Naranjo
License     : MIT
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX

The job of this interface is very simple: it forwards TeXmacs's input
to GHCi, and moves the reponses back. The protocol itself is simple,
and it's only slightly more complicated for completion.
-}

module Main where


import System.Environment ( getArgs )
import GitVersion
import Launch


main :: IO ()
main = do
  args <- getArgs
  case args of
    ("cabal" : projectPath : args')  ->  launch (CabalRepl projectPath) args'
    ("stack" : projectPath : args')  ->  launch (StackRepl projectPath) args'
    ("ghci"                : args')  ->  launch GHCiRepl args'
    _                               ->  infoAndUsage
  where
    infoAndUsage :: IO ()
    infoAndUsage = putStrLn $   "tm-ghci "
                            ++  gitVersion
                            ++  "\n©2022-23 Alexander Feterman Naranjo.\n\
                                \This program is not meant to be \
                                \launched from the command line.\n\
                                \Install as instructed in the \
                                \README.md to run from TeXmacs."

