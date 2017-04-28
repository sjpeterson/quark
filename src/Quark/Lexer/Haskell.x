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

{
{-# LANGUAGE OverloadedStrings #-}

module Quark.Lexer.Haskell ( tokenizeHaskell
                           , haskellColors ) where

import Data.ByteString.UTF8 (ByteString)

import Quark.Types (Token)
import Quark.Colors
import Quark.Lexer.Core
}

-- Alex stuff
$newline    = [\n\r]
$whitespace = [\ \t\f\v]
$digit      = [0-9]
$lower      = [a-z]
$upper      = [A-Z]
$alpha      = [$lower $upper]

haskell :-
    $newline            { \s -> Newline s}
    $whitespace+        { \s -> Whitespace s}

    "case"              { ReservedIdent "case" }
    "class"             { ReservedIdent "class" }
    "data"              { ReservedIdent "data" }
    "default"           { ReservedIdent "default " }
    "deriving"          { ReservedIdent "deriving" }
    "do"                { ReservedIdent "do" }
    "else"              { ReservedIdent "else" }
    "foreign"           { ReservedIdent "foreign" }
    "if"                { ReservedIdent "if" }
    "import"            { ReservedIdent "import" }
    "in"                { ReservedIdent "in" }
    "infix"             { ReservedIdent "infix" }
    "infixl"            { ReservedIdent "infixl" }
    "infixr"            { ReservedIdent "infixr" }
    "instance"          { ReservedIdent "instance" }
    "let"               { ReservedIdent "let" }
    "module"            { ReservedIdent "module" }
    "newtype"           { ReservedIdent "newtype" }
    "of"                { ReservedIdent "of" }
    "then"              { ReservedIdent "then" }
    "type"              { ReservedIdent "type" }
    "where"             { ReservedIdent "where" }
    "as"                { ReservedIdent "as" }
    "qualified"         { ReservedIdent "qualified" }
    "_"                 { ReservedIdent "_" }

  .                 { \s -> Unclassified s}
{

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

tokenizeHaskell :: ByteString -> [Token]
tokenizeHaskell = undefined
}