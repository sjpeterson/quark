---------------------------------------------------------------
--
-- Module:      Quark.Lexer.Python
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Portable
--
-- ----------------------------------------------------------
--
-- Python lexer module
--
---------------------------------------------------------------

{
{-# LANGUAGE OverloadedStrings #-}

module Quark.Lexer.Python ( tokenizePython
                          , pythonColors ) where

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

-- Macros
@newline                    = $lineFeed | $carriageReturn $lineFeed
@backslashPair              = \\ $white* \\
@escapedDoubleQuote         = \\ $doubleQuote
@escapedSingleQuote         = \\ $singleQuote
@oneSingleQuote             = ' $notSingleQuote
@twoSingleQuotes            = '' $notSingleQuote
@oneDoubleQuote             = \" $notDoubleQuote
@twoDoubleQuotes            = \"\" $notDoubleQuote
@stringMemberLongDouble     = $anyLongStringChar | ' | @oneDoubleQuote | @twoDoubleQuotes
@stringMemberLongSingle     = $anyLongStringChar | \" | @oneSingleQuote | @twoSingleQuotes
@stringMemberShortDouble    = $anyShortStringChar | @escapedDoubleQuote | '
@stringMemberShortSingle    = $anyShortStringChar | @escapedSingleQuote | \"
@integer                    = \-? $digit+
@fractionalPart             = \. $digit+
@exponent                   = (e | E) (\+ | \-)? $digit+
@float                      = ((\-? @integer?) @fractionalPart) | @integer \.
@expFloat                   = (@integer | @float) @exponent
@number                     = @integer | @float | @expFloat

haskellTokens :-

    \@ ($notEOLCharacter*)                          { \s -> Decorator s }
    \# ($notEOLCharacter*)                          { \s -> Comment s }
    @newline                                        { \s -> Newline s }
    $tab+                                           { \s -> Tabs s }
    $properWhitespace+                              { \s -> Whitespace s }

    -- String- and character literals
    <0> {
        \"\"\" (@stringMemberLongDouble*) \"\"\"    { \s -> StringLiteral s }
        ''' (@stringMemberLongSingle*) '''          { \s -> StringLiteral s }
        \" (@stringMemberShortDouble*) \"           { \s -> StringLiteral s }
        ' (@stringMemberShortSingle*) '             { \s -> StringLiteral s }
    }

    -- Number literals
    <0> {
        @number+                                    { \s -> NumLiteral s }
    }

    -- Identifiers, reserved and otherwise
    <0> {
        "assert"                            { \_ -> ReservedIdent "assert" }
        "and"                               { \_ -> ReservedIdent "and" }
        "as"                                { \_ -> ReservedIdent "as" }
        "break"                             { \_ -> ReservedIdent "break" }
        "continue"                          { \_ -> ReservedIdent "continue" }
        "class"                             { \_ -> ReservedIdent "class" }
        "def"                               { \_ -> ReservedIdent "def" }
        "del"                               { \_ -> ReservedIdent "del" }
        "except"                            { \_ -> ReservedIdent "except" }
        "elif"                              { \_ -> ReservedIdent "elif" }
        "else"                              { \_ -> ReservedIdent "else" }
        "finally"                           { \_ -> ReservedIdent "finally" }
        "from"                              { \_ -> ReservedIdent "from" }
        "for"                               { \_ -> ReservedIdent "for" }
        "global"                            { \_ -> ReservedIdent "global" }
        "import"                            { \_ -> ReservedIdent "import" }
        "if"                                { \_ -> ReservedIdent "if" }
        "in"                                { \_ -> ReservedIdent "in" }
        "is"                                { \_ -> ReservedIdent "is" }
        "lambda"                            { \_ -> ReservedIdent "lambda" }
        "nonlocal"                          { \_ -> ReservedIdent "nonlocal" }
        "not"                               { \_ -> ReservedIdent "not" }
        "or"                                { \_ -> ReservedIdent "or" }
        "pass"                              { \_ -> ReservedIdent "pass" }
        "return"                            { \_ -> ReservedIdent "return" }
        "raise"                             { \_ -> ReservedIdent "raise" }
        "try"                               { \_ -> ReservedIdent "try" }
        "while"                             { \_ -> ReservedIdent "while" }
        "with"                              { \_ -> ReservedIdent "with" }
        "yield"                             { \_ -> ReservedIdent "yield" }
        "False"                             { \_ -> ReservedIdent "False" }
        "None"                              { \_ -> ReservedIdent "None" }
        "True"                              { \_ -> ReservedIdent "True" }
        $idchar+                            { \s -> VarIdent s }

    }

    -- Operators and other symbols
    <0> {
       "**"                             { \_ -> Operator "**" }
       "//"                             { \_ -> Operator "//" }
       ">>"                             { \_ -> Operator ">>" }
       "<<"                             { \_ -> Operator "<<" }
       ">="                             { \_ -> Operator ">=" }
       "<="                             { \_ -> Operator "<=" }
       "=="                             { \_ -> Operator "==" }
       "!="                             { \_ -> Operator "!=" }
       "+"                              { \_ -> Operator "+" }
       "-"                              { \_ -> Operator "-" }
       "*"                              { \_ -> Operator "*" }
       "/"                              { \_ -> Operator "/" }
       "&"                              { \_ -> Operator "&" }
       "|"                              { \_ -> Operator "|" }
       "^"                              { \_ -> Operator "^" }
       "~"                              { \_ -> Operator "~" }
       ">"                              { \_ -> Operator ">" }
       "<"                              { \_ -> Operator "<" }
       "@"                              { \_ -> Operator "@" }
       ","                              { \_ -> Separator "," }
    }

    -- anything else
    .                                   { \s -> Unclassified s}

{
tokenizePython :: T.Text -> [Token]
tokenizePython = splitMultiline . tokenizePython'

tokenizePython' :: T.Text -> [Token]
tokenizePython' s = loop ('\n', [], s)
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
    t                 -> t:(splitMultiline xs)
  where
    lineSplit cons s = intersperse (Newline "\n") $ map cons (T.lines s)

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
}
