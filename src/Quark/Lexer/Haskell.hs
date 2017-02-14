{-# LANGUAGE OverloadedStrings #-}

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

import Data.ByteString (ByteString)

import Quark.Types
import Quark.Lexer.Core

haskellGrammar :: Grammar
haskellGrammar = [ (Newline, "\\A\n")
                 , (Whitespace, "\\A[ \t]*")
                 , (StringLiteral, "\\A\"[\\S\\s]*?(\")(?<!\\\\\\\")")
                 , (Pragma, "\\A\\{-#.*?#-\\}")
                 , (Comment, "\\A\\{-[\\S\\s]*?-\\}")
                 , (Comment, "\\A--[^\n]*")
                 , (CharLiteral, "\\A'[^']'")
                 , (NumLiteral, "\\A[0-9]+?\\.?[0-9]*?")
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
                 , (Separator, "\\A,")
                 , (ReservedIdent, listToRe [ "case"
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
                 , (TypeIdent, "\\A(([A-Z][A-Za-z0-9]*)+)(\\.[A-Z][A-Za-z0-9]*)*\
                                 \(?![a-zA-Z0-9\\.])")
                 , (VarIdent, "\\A([A-Z][\\w]*\\.)?([a-z][A-Za-z0-9]*)") ]

tokenizeHaskell :: ByteString -> [Token]
tokenizeHaskell = lexer $ compileGrammar haskellGrammar