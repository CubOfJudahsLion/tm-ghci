{-|
Module      : ColorizeHs
Description : Colorizes Haskell source code for [TeXmacs](https://www.texmacs.org/tmweb/home/welcome.en.html)
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : GPL-3
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX

@ColorizeHs@ tokenizes and partially (minimally) parses
the source to apply correct colorization depending on
context (calculated from the token themselves.) For
instance, it can distinguish between type names, module
names, and constructor names. As output, it produces
/TeXmacs/-format, which is plain text with macros.
-}


module ColorizeHs ( main ) where


main :: IO ()
main = putStrLn "Not implemented yet."

