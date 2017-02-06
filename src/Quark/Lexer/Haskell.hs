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
import Quark.Lexer.Core

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



tokenizeHaskell :: String -> [Token]
tokenizeHaskell = lexer haskellGrammar
