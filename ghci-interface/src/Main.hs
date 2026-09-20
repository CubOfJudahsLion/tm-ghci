{- |
    Module      : Main
    Description : Simple /GHCi/ plugin for /TeXmacs/, main module
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    This plugin allows running /GHCi/ sessions inside /TeXmacs/.
    Currently, it provides only basic functionality, i.e., no project
    options (@stack repl@, @cabal repl@.) Its only amenity is not
    blocking @.ghci@ execution, allowing any customizations to show
    up in /TeXmacs/.
-}


module Main where


import Control.Exception ( IOException, catch )
import GHCi.Control.IO.Types ( GHCiHandles(..) )
import TmGHCi.Control.IO.Bridging
import System.Exit ( die )
import System.Process ( CreateProcess(..)
                      , StdStream(CreatePipe)
                      , proc
                      , withCreateProcess
                      )


-- |  The @main@ function just spawns the child /GHCi/ process and
--    invokes the I/O Loop (see 'TmGHCi.Control.IO.Bridging.mainLoop'.)
main :: IO ()
main = do
  let procDesc  = (proc "ghci" ["-fdiagnostics-color=never"])
                    { std_in  = CreatePipe
                    , std_out = CreatePipe
                    , std_err = CreatePipe
                    }
  withCreateProcess procDesc
                    (\ !maybeIn !maybeOut !maybeErr _ ->
                        case (maybeIn, maybeOut, maybeErr) of
                          (Just ghciIn, Just ghciOut, Just ghciErr) -> do
                            catch
                              (mainLoop (GHCiHandles {ghciIn, ghciOut, ghciErr}))
                              (\(_ :: IOException) -> die "Connection to GHCi terminated")
                          _                                         ->
                            die "Could not create pipes.")

