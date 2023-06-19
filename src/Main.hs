{-|
Module      : Main
Description : Entry point to the GHCi/TeXmacs interface
Copyright   : (c) 2022,2023 Alexander Feterman Naranjo
License     : MTI
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX

The job of this interface is very simple: it fowards TeXmacs's input
to GHCi, and moves the reponses back. The protocol itself is simple,
and it's only slightly more complicated for completion.
-}

module Main where


import GitVersion


main :: IO ()
main = putStrLn $ "Hello, tm-ghci version " ++ show gitVersion ++ "."
