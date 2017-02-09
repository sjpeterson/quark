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
type Regex = String  -- This name conflicts with Text.Regex.*

{- A grammar is a list of (Token data constructor, regex) tuples in order of
   precedence, see for example Quark.Lexer.Haskell for an example -}
type Grammar = [((String -> Token), Regex)]

data Token = Comment String
           | DocComment String
           | Pragma String
           | TypeIdent String
           | VarIdent String
           | ReservedIdent String
           | Operator String
           | Bracket String
           | Separator String
           | IntLiteral String
           | FloatLiteral String
           | NumLiteral String
           | StringLiteral String
           | CharLiteral String
           | BoolLiteral String
           | Whitespace String
           | Newline String
           | Unclassified String deriving (Show, Eq)