--------
--
-- Module:      Quark.Types
-- Author:      Stefan Peterson
-- License:     MIT License
--
-- Maintainer:  Stefan Peterson (stefan.j.peterson@gmail.com)
-- Stability:   Stable
-- Portability: Unknown
--
--------
--
-- A collection of types and type synonyms that help readability
--
--------

module Quark.Types where

-- Type aliases for primitive types
type Row = Int
type Col = Int
type Index = Int
type Selection = Int

type Clipboard = String
type Name = String

-- Type aliases for tuples
type Deletion = (Int, Int)    -- (backspaces, deletes)
type Cursor = (Row, Col)
type Size = (Row, Col)
type Offset = (Row, Col)
type PrintRange = (Size, Offset)
type Option a = (Char, String, a)

-- Algebraic data types
data Direction = Backward | Forward | Up | Down deriving (Show, Eq)

-- Types and synonyms for lexers
type Regex = String  -- This name conflicts with Text.Regex..

data Grammar' = Grammar' { blockComment :: (String, String)
                         , lineComment :: String
                         , typeStart :: [Char]
                         , stringQuote :: [Char]
                         , charQuote :: [Char]
                         , numberLiteral :: [Char] }

{- A grammar is a list of (Token data constructor, regex) tuples in order of
   precedence. -}
type Grammar = [((String -> Token), Regex)]

data Token = Keyword String
           | Literal String
           | Whitespace String
           | Newline String
           | Operator String
           | Comment String
           | StringLiteral String
           | NumberLiteral String
           | IntegerLiteral String
           | FloatLiteral String
           | CharacterLiteral String
           | BooleanLiteral String
           | Unclassified String deriving (Show, Eq)