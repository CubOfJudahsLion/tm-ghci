{- |
    Module      : GHCi.Data.String.Utils
    Description : String utilities for processing /GHCi/-related strings
    Copyright   : (c) Alexander Feterman Naranjo, 2023-2026
    License     : GPL-3-or-later
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX

    String utilities for processing /GHCi/-related strings.
-}

module GHCi.Data.String.Utils ( censorQuitCommand ) where


import GHCi.Data.String.Utils.Parsers ( firstParseOr, quitCommandParser )


--  Determines whether a string will be read by /GHCi/
--  as a @:quit@ command.
isQuitCommand :: String -> Bool
isQuitCommand = firstParseOr quitCommandParser False


-- |  If the provided 'String' -- assumed to be from user input --
--    is recognized as the @:quit@ command (or any of its allowed
--    variants), supress it and cause /GHCi/ to remind them of the
--    proper way to end the session.
censorQuitCommand :: String -> String
censorQuitCommand s = if isQuitCommand s
                        then  "System.IO.hPutStrLn System.IO.stderr \"\
                              \Right-click and choose \\\"Close Session\\\"\
                              \ to finish.\"\n"
                        else  s

