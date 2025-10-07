{-|
Module      : Data.Text.Utils.Quote
Description : Utilities to quote 'Text's.
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : GPL-3
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX
-}


{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Utils.Quote where


import Data.Text ( Text, append, replace )


-- |  Quotes any string. Inside quotes and backslashes are preceded with a backslash.
quoteText :: Text -> Text
quoteText = flip append "\"" . append "\"" . replace "\"" "\\\"" . replace "\\" "\\\\"
  
