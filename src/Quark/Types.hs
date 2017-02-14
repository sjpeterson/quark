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

import qualified Data.ByteString.Char8 as B
import qualified Text.Regex.PCRE as R

-- Type aliases for primitive types
type Row = Int
type Col = Int
type Index = Int
type Selection = Int

type Clipboard = B.ByteString
type Name = B.ByteString

-- Type aliases for tuples
type Deletion = (Int, Int)    -- (backspaces, deletes)
type Cursor = (Row, Col)
type Size = (Row, Col)
type Offset = (Row, Col)
type PrintRange = (Size, Offset)
type Option a = (Char, B.ByteString, a)

-- Algebraic data types
data Direction = Backward | Forward | Up | Down deriving (Show, Eq)

-- Types and synonyms for lexers
type Regex = B.ByteString  -- This name conflicts with Text.Regex.*

{- A grammar is a list of (Token data constructor, regex) tuples in order of
   precedence, see for example Quark.Lexer.Haskell for an example -}
type Grammar = [((B.ByteString -> Token), Regex)]
type CompiledGrammar = [((B.ByteString -> Token), R.Regex)]

data Token = Comment B.ByteString
           | DocComment B.ByteString
           | Pragma B.ByteString
           | TypeIdent B.ByteString
           | VarIdent B.ByteString
           | ReservedIdent B.ByteString
           | Operator B.ByteString
           | Bracket B.ByteString
           | Separator B.ByteString
           | IntLiteral B.ByteString
           | FloatLiteral B.ByteString
           | NumLiteral B.ByteString
           | StringLiteral B.ByteString
           | CharLiteral B.ByteString
           | BoolLiteral B.ByteString
           | Whitespace B.ByteString
           | Newline B.ByteString
           | Unclassified B.ByteString deriving (Show, Eq)