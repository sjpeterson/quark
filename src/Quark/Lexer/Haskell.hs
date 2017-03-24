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

module Quark.Lexer.Haskell ( tokenizeHaskell, haskellColors ) where

import Data.ByteString.UTF8 (ByteString)

import Quark.Types
import Quark.Colors
import Quark.Lexer.Core

haskellGrammar :: Grammar
haskellGrammar = [ (Newline, "\\A\n")
                 , (Whitespace, "\\A[ \t]*")
                 , (StringLiteral, "\\A\"[\\S\\s]*?(\")(?<!\\\\\\\")")
                 , (Pragma, "\\A\\{-#.*?#-\\}")
                 , (DocComment, "\\A\\{-|[\\S\\s]*?-\\}")
                 , (Comment, "\\A\\{-[\\S\\s]*?-\\}")
                 , (DocComment, "\\A-- |[^\n]*")
                 , (Comment, "\\A--[^\n]*")
                 , (CharLiteral, "\\A'([^'\n]|\\\\[a-z]|\\\\[A-Z0-9]+)'")
                 , (NumLiteral, "\\A[0-9]+\\.?[0-9]*")
                 , (Operator, listToRe' [ "::"
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
                                            , "as"
                                            , "qualified"
                                            , "_" ])
                 , (TypeIdent, "\\A(([A-Z]\\w*)+)(\\.[A-Z]\\w*)*\
                                 \(?![a-zA-Z0-9\\.])")
                 , (VarIdent, "\\A([A-Z]\\w*\\.)*([a-z]\\w*)") ]

haskellColors :: Token -> Int
haskellColors (ReservedIdent _) = orange
haskellColors (Operator _)      = orange
haskellColors (Separator _)     = orange
haskellColors (TypeIdent _)     = purple
haskellColors (StringLiteral _) = lightGreen
haskellColors (CharLiteral _)   = teal
haskellColors (NumLiteral _)    = lightBlue
haskellColors (Pragma _)        = yellow
haskellColors (DocComment _)    = green
haskellColors (Comment _)       = lightGray
haskellColors _                 = defaultColor

compiledHaskellGrammar :: CompiledGrammar
compiledHaskellGrammar = compileGrammar haskellGrammar

tokenizeHaskell :: ByteString -> [Token]
tokenizeHaskell = lexer $ compiledHaskellGrammar