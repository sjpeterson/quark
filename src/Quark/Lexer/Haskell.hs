---------------------------------------------------------------
--
-- Module:      Quark.Lexer.Haskell
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Haskell lexer module
--
---------------------------------------------------------------

module Quark.Lexer.Haskell ( tokenizeHaskell ) where

import Data.List ( intercalate )

import Text.Regex.PCRE

import Quark.Types

haskellGrammar = [ (StringLiteral, "\".*?\"")
                 , (Comment, "\\{-.*-\\}")
                 , (Comment, "--.*?\n")
                 , (CharacterLiteral, "'[^']'")
                 , (NumberLiteral, "[0-9]+?\\.?[0-9]*?")
                 , (Operator, intercalate "|" [ "::"
                                              , "=>"
                                              , "->"
                                              , "="
                                              , "!"
                                              , ";"
                                              , "\\|"
                                              , "<-"
                                              , ">"
                                              , "<"
                                              , "<="
                                              , ">="
                                              , "=="
                                              , "@"
                                              , "\\\\"]) ]

--  , blockComment     = "\\{-.*-\\}"
--  , lineComment      = "--.*?\n"
--  , stringLiteral    = "\".*\""
--  , characterLiteral = "'[^']'"
--  , numberLiteral    = "[0-9]+?\\.?[0-9]*?"
--  , typeLiteral      = "(([A-Z]\\w*)+)(\\.[A-Z]\\w*)*(?![\\w\\.])"
--  , identifier       = "[a-z]\\w*"
--  , whitespace       = "[ \t]+"
--  , operator         = intercalate "|" [ "::"
--                                       , "=>"
--                                       , "->"
--                                       , "="
--                                       , "!"
--                                       , ";"
--                                       , "\\|"
--                                       , "<-"
--                                       , ">"
--                                       , "<"
--                                       , "<="
--                                       , ">="
--                                       , "=="
--                                       , "@"
--                                       , "\\\\"] }


lexer :: Grammar -> String -> [Token]
lexer g s = undefined

tokenizeHaskell :: String -> [Token]
tokenizeHaskell = lexer haskellGrammar
