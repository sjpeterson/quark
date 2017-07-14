---------------------------------------------------------------
--
-- Module:      Quark.Lexer.Clojure
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
-- ----------------------------------------------------------
--
-- Clojure lexer module
--
---------------------------------------------------------------

{
{-# LANGUAGE OverloadedStrings #-}

module Quark.Lexer.Clojure ( tokenizeClojure
                           , clojureColors ) where

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
$singleQuote          = '
$notDoubleQuote       = [.] # \"
$notSingleQuote       = [.] # '
$digit                = [0-9]
$lower                = [a-z]
$upper                = [A-Z]
$upperOrDigit         = [$upper $digit]
$alpha                = [$lower $upper]
$idchar               = [$alpha $digit \. _]
$anyShortStringChar   = [.] # [' \"]
$anyLongStringChar    = [. \n] # [' \"]
$bracket              = [\[ \] \( \) \{ \}]


-- Macros
@newline                    = $lineFeed | $carriageReturn $lineFeed
@backslashPair              = \\ $white* \\
@escapedDoubleQuote         = \\ $doubleQuote
@stringMember               = $notDoubleQuote | @backslashPair | @escapedDoubleQuote
@integer                    = \-? $digit+
@fractionalPart             = \. $digit+
@exponent                   = (e | E) (\+ | \-)? $digit+
@float                      = ((\-? @integer?) @fractionalPart) | @integer \.
@expFloat                   = (@integer | @float) @exponent
@number                     = @integer | @float | @expFloat

clojureTokens :-

    ";" ($notEOLCharacter*)             { \s -> Comment s }
    @newline                            { \s -> Newline s }
    $tab+                               { \s -> Tabs s }
    $properWhitespace+                  { \s -> Whitespace s }

    -- String- and character literals
    <0> {
        \" (@stringMember*) \"          { \s -> StringLiteral s }
    }

    -- Number literals
    <0> {
        @number+                        { \s -> NumLiteral s }
    }

    -- Operators and other symbols
    <0> {
       "#"                              { \_ -> Decorator "#" }
       "'"                              { \_ -> Decorator "'" }
    }

    -- Brackets
    <0> {
        $bracket                        { \s -> Bracket s }
    }

    -- anything else
    .                                   { \s -> Unclassified s}

{
tokenizeClojure :: T.Text -> [Token]
tokenizeClojure = splitMultiline . tokenizeClojure'

tokenizeClojure' :: T.Text -> [Token]
tokenizeClojure' s = loop ('\n', [], s)
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
    (DocComment s)    -> (lineSplit DocComment s) ++ (splitMultiline xs)
    t                 -> t:(splitMultiline xs)
  where
    lineSplit cons s = intersperse (Newline "\n") $ map cons (T.lines s)

clojureColors :: Token -> Int
clojureColors (StringLiteral _) = lightGreen
clojureColors (NumLiteral _)    = lightBlue
clojureColors (Comment _)       = lightGray
clojureColors (Decorator _)     = orange
clojureColors _                 = defaultColor
}