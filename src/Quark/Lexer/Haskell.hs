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

import Quark.Types
import Quark.Lexer.Core

haskellGrammar = [ (StringLiteral, "\"[\\S\\s]*?(\")(?<!\\\\\\\")")
                 , (Comment, "\\{-[\\S\\s]*?-\\}")
                 , (Comment, "--[^\n]*")
                 , (CharacterLiteral, "'[^']'")
                 , (NumberLiteral, "[0-9]+?\\.?[0-9]*?")
                 , (Operator, listToRe [ "::"
                                       , "=>"
                                       , "->"
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
                                       , "="
                                       , "\\.\\."
                                       , ":"
                                       , "~"
                                       , "\\\\"])
                 , (Separator, ",")
                 , (Keyword, listToRe [ "case"
                                      , "class"
                                      , "data"
                                      , "default"
                                      , "deriving"
                                      , "do"
                                      , "else"
                                      , "foreign"
                                      , "if"
                                      , "import"
                                      , "in"
                                      , "infix"
                                      , "infixl"
                                      , "infixr"
                                      , "instance"
                                      , "let"
                                      , "module"
                                      , "newtype"
                                      , "of"
                                      , "then"
                                      , "type"
                                      , "where"
                                      , "_" ])
                 , (TypeIdent, "(([A-Z][A-Za-z0-9]*)+)(\\.[A-Z][A-Za-z0-9]*)*\
                                 \(?![a-zA-Z0-9\\.])")
                 , (VarIdent, "([A-Z][A-Za-z0-9]*\\.)?([a-z][A-Za-z0-9]*)")
                 , (Newline, "\n") ]

tokenizeHaskell :: String -> [Token]
tokenizeHaskell = lexer haskellGrammar