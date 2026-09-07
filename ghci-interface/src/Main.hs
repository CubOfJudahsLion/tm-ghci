{- |
    Module      : Main
    Description : Simple GHCi plugin for TeXmacs, main module.
    Copyright   : (c) Alexander Feterman Naranjo, 2023-26
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    This plugin allows running /GHCi/ sessions inside /TeXmacs/.
    Currently, it provides only basic functionality, i.e., no project
    options (stack repl, cabal repl.) Its only amenity is the use of
    the actual /GHCi/ prompt, allowing customizations to be seen in
    /TeXmacs/.
-}

{-# LANGUAGE GHC2021 #-}

module Main where

import Control.Exception ( IOException, catch )
import System.IO
import System.Exit ( die )
import System.Process ( shell, CreateProcess(..), StdStream(CreatePipe), withCreateProcess )
import TeXmacs.Control.IO


-- |  The @main@ function spawns the child GHCi process and
--    invokes the I/O Loop ('TeXmacs.Control.IO.mainLoop'.)
main :: IO ()
main = do
  let procDesc  = (shell "ghci -fdiagnostics-color=never 2>&1")
                    { std_in  = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }
  withCreateProcess procDesc
                    (\ !maybeIn !maybeOut !maybeErr _ ->
                        case (maybeIn, maybeOut, maybeErr) of
                          (Just ghciIn, Just ghciOut, Just ghciErr) ->
                            catch
                              (mainLoop (GHCiHandles {ghciIn, ghciOut, ghciErr}))
                              (\(_ :: IOException) -> die "Connection to GHCi terminated")
                          _                                         ->
                            die "Could not create pipes.")

