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
$commentChar          = [. \n] # \-
$notRightCurlyBracket = [. \n] # \}
$digit                = [0-9]
$lower                = [a-z]
$upper                = [A-Z]
$upperOrDigit         = [$upper $digit]
$alpha                = [$lower $upper]
$idchar               = [$alpha $digit \']


-- Macros
@newline            = $lineFeed | $carriageReturn $lineFeed
@backslashPair      = \\ $white* \\
@escapedDoubleQuote = \\ $doubleQuote
@stringMember       = $notDoubleQuote | @backslashPair | @escapedDoubleQuote
@nonClosingDash     = \- $notRightCurlyBracket
@commentMember      = $commentChar | @nonClosingDash
@varid              = $lower $idchar*
@conid              = $upper $idchar*
@qid                = @conid .

haskellTokens :-

    "--|" ($notEOLCharacter*)           { \s -> DocComment s }
    "--" ($notEOLCharacter*)            { \s -> Comment s }
    @newline                            { \s -> Newline s }
    $tab+                               { \s -> Tabs s }
    $properWhitespace+                  { \s -> Whitespace s }

    -- String- and character literals
    <0> {
        \" (@stringMember*) \"          { \s -> StringLiteral s }
        ' $printable '                  { \s -> CharLiteral s }
        "'\" $upperOrDigit{2,3} '       { \s -> CharLiteral s }

    }

    -- Multiline comments and pragmas
    <0> {
        "{-#" ($notEOLCharacter*) "#-}" { \s -> Pragma s }
        "{-|" @commentMember* "-}"      { \s -> DocComment s }
        "{-" @commentMember* "-}"       { \s -> Comment s }
    }

    -- Number literals
    <0> {
        $digit+                         { \s -> NumLiteral s }
    }

    -- Identifiers, reserved and otherwise
    <0> {
        "as"                            { \_ -> ReservedIdent "as" }
        "class"                         { \_ -> ReservedIdent "class" }
        "case"                          { \_ -> ReservedIdent "case" }
        "deriving"                      { \_ -> ReservedIdent "deriving" }
        "default"                       { \_ -> ReservedIdent "default" }
        "data"                          { \_ -> ReservedIdent "data" }
        "do"                            { \_ -> ReservedIdent "do" }
        "else"                          { \_ -> ReservedIdent "else" }
        "foreign"                       { \_ -> ReservedIdent "foreign" }
        "hiding"                        { \_ -> ReservedIdent "hiding" }
        "instance"                      { \_ -> ReservedIdent "instance" }
        "infixl"                        { \_ -> ReservedIdent "infixl" }
        "infixr"                        { \_ -> ReservedIdent "infixr" }
        "import"                        { \_ -> ReservedIdent "import" }
        "infix"                         { \_ -> ReservedIdent "infix" }
        "if"                            { \_ -> ReservedIdent "if" }
        "in"                            { \_ -> ReservedIdent "in" }
        "let"                           { \_ -> ReservedIdent "let" }
        "module"                        { \_ -> ReservedIdent "module" }
        "newtype"                       { \_ -> ReservedIdent "newtype" }
        "of"                            { \_ -> ReservedIdent "of" }
        "qualified"                     { \_ -> ReservedIdent "qualified" }
        "then"                          { \_ -> ReservedIdent "then" }
        "type"                          { \_ -> ReservedIdent "type" }
        "where"                         { \_ -> ReservedIdent "where" }
        "_"                             { \_ -> ReservedIdent "_" }
        @qid* @conid                    { \s -> TypeIdent s }
        @qid* @varid                    { \s -> VarIdent s }

    }

    -- Operators and other symbols
    <0> {
       ".."                             { \_ -> Operator ".." }
       "\\"                             { \_ -> Operator "\\" }
       "::"                             { \_ -> Operator "::" }
       "=>"                             { \_ -> Operator "=>" }
       "->"                             { \_ -> Operator "->" }
       "<-"                             { \_ -> Operator "<-" }
       "<="                             { \_ -> Operator "<=" }
       ">="                             { \_ -> Operator ">=" }
       "=="                             { \_ -> Operator "==" }
       "|"                              { \_ -> Operator "|" }
       "@"                              { \_ -> Operator "@" }
       "="                              { \_ -> Operator "=" }
       "!"                              { \_ -> Operator "!" }
       ":"                              { \_ -> Operator ":" }
       ";"                              { \_ -> Operator ";" }
       "~"                              { \_ -> Operator "~" }
       ">"                              { \_ -> Operator ">" }
       "<"                              { \_ -> Operator "<" }
       ","                              { \_ -> Separator "," }
    }

    -- catchall
    .                                   { \s -> Unclassified s}

{
tokenizeHaskell :: T.Text -> [Token]
tokenizeHaskell = splitMultiline . tokenizeHaskell'

tokenizeHaskell' :: T.Text -> [Token]
tokenizeHaskell' s = loop ('\n', [], s)
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
}
