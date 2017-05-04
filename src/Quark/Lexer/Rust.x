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

{
{-# LANGUAGE OverloadedStrings #-}

module Quark.Lexer.Rust ( tokenizeRust
                        , rustColors ) where

import qualified Data.Text as T
import Data.List ( intersperse )

import Quark.Types
import Quark.Colors
import Quark.Lexer.Alex ( AlexInput
                        , alexGetByte
                        , alexInputPrevChar )

}

-- Character sets
$lineFeed             = \n
$carriageReturn       = \r
$eolCharacter         = [$lineFeed $carriageReturn]
$notEOLCharacter      = ~$eolCharacter
$properWhitespace     = [\ ]
$tab                  = \t
$doubleQuote          = \"
$notDoubleQuote       = [.] # \"
$commentChar          = [. \n]
$digit                = [0-9]
$lower                = [a-z]
$upper                = [A-Z]
$upperOrDigit         = [$upper $digit]
$alpha                = [$lower $upper]
$idchar               = [$alpha $digit \' _]
$bracket              = [\[ \] \( \) \{ \}]


-- Macros
@newline            = $lineFeed | $carriageReturn $lineFeed
@backslashPair      = \\ $white* \\
@escapedDoubleQuote = \\ $doubleQuote
@stringMember       = $notDoubleQuote | @backslashPair | @escapedDoubleQuote
@integer            = \-? $digit+
@fractionalPart     = \. $digit+
@exponent           = (e | E) (\+ | \-)? $digit+
@float              = (@integer? @fractionalPart) | @integer \.
@expFloat           = (@integer | @float) @exponent
@number             = @integer | @float | @expFloat

RustTokens :-

    "///" ($notEOLCharacter*)           { \s -> DocComment s }
    "//!" ($notEOLCharacter*)           { \s -> Comment s }
    "//" ($notEOLCharacter*)            { \s -> Comment s }
    @newline                            { \s -> Newline s }
    $tab+                               { \s -> Tabs s }
    $properWhitespace+                  { \s -> Whitespace s }

    -- String- and character literals
    <0> {
        \" (@stringMember*) \"          { \s -> StringLiteral s }
        ' $printable '                  { \s -> CharLiteral s }
        "'\" $upperOrDigit{2,3} '       { \s -> CharLiteral s }

    }

    -- Multiline comments
    <0> {
        "/*" $commentChar* "*/"         { \s -> Comment s }
    }

    -- Number literals
    <0> {
        @number+                        { \s -> NumLiteral s }
    }

    -- Identifiers, reserved and otherwise
    <0> {
        "abstract"                      { \_ -> ReservedIdent "abstract" }
        "alignof"                       { \_ -> ReservedIdent "alignof" }
        "as"                            { \_ -> ReservedIdent "as" }
        "become"                        { \_ -> ReservedIdent "become" }
        "break"                         { \_ -> ReservedIdent "break" }
        "box"                           { \_ -> ReservedIdent "box" }
        "continue"                      { \_ -> ReservedIdent "continue" }
        "crate"                         { \_ -> ReservedIdent "crate" }
        "const"                         { \_ -> ReservedIdent "const" }
        "do"                            { \_ -> ReservedIdent "do" }
        "extern"                        { \_ -> ReservedIdent "extern" }
        "else"                          { \_ -> ReservedIdent "else" }
        "enum"                          { \_ -> ReservedIdent "enum" }
        "false"                         { \_ -> ReservedIdent "false" }
        "final"                         { \_ -> ReservedIdent "final" }
        "for"                           { \_ -> ReservedIdent "for" }
        "fn"                            { \_ -> ReservedIdent "fn" }
        "impl"                          { \_ -> ReservedIdent "impl" }
        "if"                            { \_ -> ReservedIdent "if" }
        "in"                            { \_ -> ReservedIdent "in" }
        "loop"                          { \_ -> ReservedIdent "loop" }
        "let"                           { \_ -> ReservedIdent "let" }
        "macro"                         { \_ -> ReservedIdent "macro" }
        "match"                         { \_ -> ReservedIdent "match" }
        "move"                          { \_ -> ReservedIdent "move" }
        "mod"                           { \_ -> ReservedIdent "mod" }
        "mut"                           { \_ -> ReservedIdent "mut" }
        "offsetof"                      { \_ -> ReservedIdent "offsetof" }
        "override"                      { \_ -> ReservedIdent "override" }
        "priv"                          { \_ -> ReservedIdent "priv" }
        "sizeof"                        { \_ -> ReservedIdent "sizeof" }
        "static"                        { \_ -> ReservedIdent "static" }
        "struct"                        { \_ -> ReservedIdent "struct" }
        "super"                         { \_ -> ReservedIdent "super" }
        "Self"                          { \_ -> ReservedIdent "Self" }
        "self"                          { \_ -> ReservedIdent "self" }
        "typeof"                        { \_ -> ReservedIdent "typeof" }
        "trait"                         { \_ -> ReservedIdent "trait" }
        "true"                          { \_ -> ReservedIdent "true" }
        "type"                          { \_ -> ReservedIdent "type" }
        "unsized"                       { \_ -> ReservedIdent "unsized" }
        "unsafe"                        { \_ -> ReservedIdent "unsafe" }
        "use"                           { \_ -> ReservedIdent "use" }
        "virtual"                       { \_ -> ReservedIdent "virtual" }
        "where"                         { \_ -> ReservedIdent "where" }
        "while"                         { \_ -> ReservedIdent "while" }
        "yield"                         { \_ -> ReservedIdent "yield" }
        $idchar+                        { \s -> VarIdent s }
    }

    -- Operators and other symbols
    <0> {
       "::"                             { \_ -> Symbol "::" }
       "->"                             { \_ -> Symbol "->" }
       "=>"                             { \_ -> Symbol "=>" }
       "#"                              { \_ -> Symbol "#" }
       ";"                              { \_ -> Symbol ";" }
       "&&"                             { \_ -> Operator "&&" }
       "||"                             { \_ -> Operator "||" }
       ">>"                             { \_ -> Operator ">>" }
       "<<"                             { \_ -> Operator "<<" }
       ">="                             { \_ -> Operator ">=" }
       "<="                             { \_ -> Operator "<=" }
       "=="                             { \_ -> Operator "==" }
       "!="                             { \_ -> Operator "!=" }
       "&"                              { \_ -> Operator "&" }
       "|"                              { \_ -> Operator "|" }
       "="                              { \_ -> Operator "=" }
       "!"                              { \_ -> Operator "!" }
       "%"                              { \_ -> Operator "%" }
       "+"                              { \_ -> Operator "+" }
       "-"                              { \_ -> Operator "-" }
       "*"                              { \_ -> Operator "*" }
       "/"                              { \_ -> Operator "/" }
       "^"                              { \_ -> Operator "^" }
       ">"                              { \_ -> Operator ">" }
       "<"                              { \_ -> Operator "<" }
       ","                              { \_ -> Separator "," }
    }

    -- Brackets
    <0> {
        $bracket                        { \s -> Bracket s }
    }

    -- anything else
    .                                   { \s -> Unclassified s}

{
tokenizeRust :: T.Text -> [Token]
tokenizeRust = splitMultiline . tokenizeRust'

tokenizeRust' :: T.Text -> [Token]
tokenizeRust' s = loop ('\n', [], s)
  where
    loop input@(_, _bs, s') = case alexScan input 0 of
        AlexEOF -> []
        AlexError _ -> error "lexical error"
        AlexSkip input' len      -> loop input'
        AlexToken input' len act -> (act $ T.take len s'):(loop input')

splitMultiline :: [Token] -> [Token]
splitMultiline [] = []
splitMultiline (x:xs) = case x of
    (StringLiteral s) -> (lineSplit StringLiteral s) ++ (splitMultiline xs)
    (Comment s)       -> (lineSplit Comment s) ++ (splitMultiline xs)
    t                 -> t:(splitMultiline xs)
  where
    lineSplit cons s = intersperse (Newline "\n") $ map cons (T.lines s)

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
}
