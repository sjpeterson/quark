{-# LANGUAGE OverloadedStrings #-}

---------------------------------------------------------------
--
-- Module:      Quark.Lexer.Rust
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Rust lexer module
--
---------------------------------------------------------------

module Quark.Lexer.Rust ( tokenizeRust, rustColors ) where

import Data.ByteString.UTF8 (ByteString)

import Quark.Types
import Quark.Colors
import Quark.Lexer.Core

rustGrammar :: Grammar
rustGrammar = [ (Newline, "\\A\n")
              , (Whitespace, "\\A *")
              , (Tabs, "\\A\t*")
              , (StringLiteral, "\\A\"[\\S\\s]*?(\")(?<!\\\\\\\")")
              , (Comment, "\\A/\\*[\\S\\s]*?\\*/")
              , (DocComment, "\\A//(/||!)[^\n]*")
              , (Comment, "\\A//[^\n]*")
              , (CharLiteral, "\\A'[^']'")
              , (NumLiteral, "\\A[0-9]+\\.?[0-9]*((e-?)[0-9]*)?")
              , (Symbol, listToRe [ "::"
                                  , "->"
                                  , "=>"
                                  , "#"
                                  , ";"])
              , (Operator, listToRe' [ "-"
                                     , "\\*"
                                     , "!"
                                     , "\\+"
                                     , "/"
                                     , "%"
                                     , "&"
                                     , "\\|"
                                     , "\\^"
                                     , "<<"
                                     , ">>"
                                     , "&&"
                                     , "\\|\\|"
                                     , "=="
                                     , "!="
                                     , "<"
                                     , ">"
                                     , "<="
                                     , ">="])
              , (Separator, "\\A,")
              , (Bracket, listToRe [ "\\["
                                   , "\\]"
                                   , "\\("
                                   , "\\)"
                                   , "\\{"
                                   , "\\}"])
              , (ReservedIdent, listToRe [ "abstract"
                                         , "alingof"
                                         , "as"
                                         , "become"
                                         , "box"
                                         , "break"
                                         , "const"
                                         , "continue"
                                         , "crate"
                                         , "do"
                                         , "else"
                                         , "enum"
                                         , "extern"
                                         , "false"
                                         , "final"
                                         , "fn"
                                         , "for"
                                         , "if"
                                         , "impl"
                                         , "in"
                                         , "let"
                                         , "loop"
                                         , "macro"
                                         , "match"
                                         , "mod"
                                         , "move"
                                         , "mut"
                                         , "offsetof"
                                         , "override"
                                         , "priv"
                                         , "Self"
                                         , "self"
                                         , "sizeof"
                                         , "static"
                                         , "struct"
                                         , "super"
                                         , "trait"
                                         , "true"
                                         , "type"
                                         , "typeof"
                                         , "unsafe"
                                         , "unsized"
                                         , "use"
                                         , "virtual"
                                         , "where"
                                         , "while"
                                         , "yield"])
              , (BoolLiteral, listToRe ["true", "false"])
              , (VarIdent, "\\A([\\w]*\\.)?([\\w]*)!?") ]

rustColors :: Token -> Int
rustColors (ReservedIdent _) = orange
rustColors (DocComment _)    = green
rustColors (Operator _)      = orange
rustColors (Separator _)     = orange
rustColors (TypeIdent _)     = purple
rustColors (StringLiteral _) = lightGreen
rustColors (NumLiteral _)    = lightBlue
rustColors (Decorator _)     = yellow
rustColors (Comment _)       = lightGray
rustColors _                 = defaultColor

compiledRustGrammar :: CompiledGrammar
compiledRustGrammar = compileGrammar rustGrammar

tokenizeRust :: ByteString -> [Token]
tokenizeRust = lexer $ compiledRustGrammar