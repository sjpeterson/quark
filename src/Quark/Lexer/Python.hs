{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------------
--
-- Module:      Quark.Lexer.Python
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Python lexer module
--
---------------------------------------------------------------

module Quark.Lexer.Python ( tokenizePython, pythonColors ) where

import Data.ByteString (ByteString)

import Quark.Types
import Quark.Colors
import Quark.Lexer.Core

pythonGrammar :: Grammar
pythonGrammar = [ (Newline, "\\A\n")
                , (Whitespace, "\\A[ \t]*")
                , (DocComment, "\\A\"\"\"[\\S\\s]*?\"\"\"")
                , (StringLiteral, "\\A'''[\\s\\S]*?'''")
                , (StringLiteral, listToRe' [ "\"[^\n]*?\"(?<!\\\\)"
                                            , "'[\\s\\S]*?'(?<!\\\\)" ])
                , (Decorator, "\\A@[a-zA-Z_\\.]+")
                , (Comment, "\\A#[^\n]*")
                , (CharLiteral, "\\A'[^']'")
                , (NumLiteral, "\\A[0-9]+?\\.?[0-9]*?")
                , (Operator, listToRe' [ "\\+"
                                       , "-"
                                       , "\\*"
                                       , "\\*\\*"
                                       , "/"
                                       , "//"
                                       , "%"
                                       , "@"
                                       , "<<"
                                       , ">>"
                                       , "&"
                                       , "\\|"
                                       , "\\^"
                                       , "~"
                                       , "<"
                                       , ">"
                                       , "<="
                                       , ">="
                                       , "=="
                                       , "!=" ])
                , (Separator, "\\A,")
                , (ReservedIdent, listToRe [ "False"
                                           , "None"
                                           , "True"
                                           , "and"
                                           , "as"
                                           , "assert"
                                           , "break"
                                           , "class"
                                           , "continue"
                                           , "def"
                                           , "del"
                                           , "elif"
                                           , "else"
                                           , "except"
                                           , "finally"
                                           , "for"
                                           , "from"
                                           , "global"
                                           , "if"
                                           , "import"
                                           , "in"
                                           , "is"
                                           , "lambda"
                                           , "nonlocal"
                                           , "not"
                                           , "or"
                                           , "pass"
                                           , "raise"
                                           , "return"
                                           , "try"
                                           , "while"
                                           , "with"
                                           , "yield" ])
                , (VarIdent, "\\A([\\w]*\\.)?([\\w]*)") ]

pythonColors :: Token -> Int
pythonColors (ReservedIdent _) = orange
pythonColors (DocComment _)    = green
pythonColors (Operator _)      = orange
pythonColors (Separator _)     = orange
pythonColors (TypeIdent _)     = purple
pythonColors (StringLiteral _) = lightGreen
pythonColors (NumLiteral _)    = lightBlue
pythonColors (Decorator _)     = yellow
pythonColors (Comment _)       = lightGray
pythonColors _                 = defaultColor

compiledPythonGrammar :: CompiledGrammar
compiledPythonGrammar = compileGrammar pythonGrammar

tokenizePython :: ByteString -> [Token]
tokenizePython = lexer $ compiledPythonGrammar