{- |
    Module      : TeXmacs.Data.String.Utils
    Description : String utilities for processing TeXmacs strings
    Copyright   : (c) Alexander Feterman Naranjo, 2023-26
    License     : GPL-3
    Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
    Stability   : experimental
    Portability : POSIX
-}

module TeXmacs.Data.String.Utils ( censorQuitCommand ) where

import TeXmacs.Data.String.Utils.Parsers


--  Determines whether a string will be read by GHCi
--  as a :quit command.
isQuitCommand :: String -> Bool
isQuitCommand = firstParseOr quitCommandParser False


--  | If the provided 'String' -- assumed to be from user input --
--    is recognized as the @:quit@ command, supress it and cause
--    GHCi to remind them of the proper way to end the session.
censorQuitCommand :: String -> String
censorQuitCommand s = if isQuitCommand s
                        then "putStrLn \"Use right-click -> Close Session to finish.\"\n"
                        else s

