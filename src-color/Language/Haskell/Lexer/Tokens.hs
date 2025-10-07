{-|
Module      : Language.Haskell.Lexer.Tokens
Description : Tokens in the Haskell Language
Copyright   : (c) 2025 Alexander Feterman Naranjo
License     : GPL-3
Maintainer  : 10951848+CubOfJudahsLion@users.noreply.github.com
Stability   : experimental
Portability : POSIX

Tokens, as far as their relevance goes for pretty-printing.
-}


{-# LANGUAGE BangPatterns #-}
module Language.Haskell.Lexer.Tokens where


import Data.Text ( Text )


-- |  Types of tokens that concern a pretty-printer
data Token  = Keyword !Text
            | ReservedOperator !Text
            | Punctuation !Text
            | Pragma !Text
            | Comment !Text
            | Space !Text
            | ModuleName !Text
            | TypeName !Text
            | TypeVariableName !Text
            | ConstructorName !Text
            | VarName !Text
            | Operator !Text
            | BooleanLiteral !Text
            | NumericLiteral !Text
            | CharLiteral !Text
            | StringLiteral !Text
  deriving ( Show )

